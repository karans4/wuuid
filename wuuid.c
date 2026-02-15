/* wuuid — UUID but words
 *
 * Human-readable unique identifiers using BIP39 words.
 * Each word encodes 11 bits of entropy (2048 word list).
 *
 * UUID versions (vN):
 *   v1  timestamp + MAC address (when + where)
 *   v4  pure random (default)
 *   v5  SHA-1 of namespace + name (deterministic)
 *   v7  Unix timestamp + random (sortable)
 *
 * WUUID versions (wN):
 *   w1  phrase (grammatical templates, linguistic structure)
 *   w2  name (hash any string → deterministic prefix + random)
 *   w3  euphonic (phoneme-aware, prosodic rhythm, semantic cohesion)
 *
 * Reads from /dev/urandom. Unix-friendly: one ID per line.
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <fcntl.h>
#include <unistd.h>
#include <stdint.h>
#include <time.h>
#include <sys/time.h>
#include <ctype.h>

#ifdef __linux__
#include <net/if.h>
#include <sys/ioctl.h>
#endif

#include "wordlist.h"
#include "wordcat.h"
#include "wling.h"
#include "sha1.h"

#define WORDLIST_BITS 11
#define WORDLIST_SIZE 2048

/* ---- entropy ---- */

static int read_entropy(uint8_t *buf, size_t len) {
    int fd = open("/dev/urandom", O_RDONLY);
    if (fd < 0) return -1;
    size_t total = 0;
    while (total < len) {
        ssize_t n = read(fd, buf + total, len - total);
        if (n <= 0) { close(fd); return -1; }
        total += n;
    }
    close(fd);
    return 0;
}

/* ---- bit manipulation ---- */

static uint16_t extract_bits(const uint8_t *data, int bit_offset) {
    int byte_idx = bit_offset / 8;
    int bit_idx = bit_offset % 8;
    uint32_t val = ((uint32_t)data[byte_idx] << 16) |
                   ((uint32_t)data[byte_idx + 1] << 8) |
                   ((uint32_t)data[byte_idx + 2]);
    int shift = 24 - bit_idx - WORDLIST_BITS;
    return (val >> shift) & 0x7FF;
}

static void set_bits(uint8_t *data, int bit_offset, uint16_t val, int nbits) {
    for (int i = nbits - 1; i >= 0; i--) {
        int pos = bit_offset + (nbits - 1 - i);
        int byte_idx = pos / 8;
        int bit_idx = 7 - (pos % 8);
        if (val & (1 << i))
            data[byte_idx] |= (1 << bit_idx);
        else
            data[byte_idx] &= ~(1 << bit_idx);
    }
}

/* ---- helpers ---- */

static void emit_words(const uint8_t *data, int nwords, const char *sep) {
    for (int w = 0; w < nwords; w++) {
        uint16_t idx = extract_bits(data, w * WORDLIST_BITS);
        if (w > 0) fputs(sep, stdout);
        fputs(wordlist[idx], stdout);
    }
    putchar('\n');
}

static int word_to_index(const char *word) {
    for (int i = 0; i < WORDLIST_SIZE; i++)
        if (strcmp(wordlist[i], word) == 0) return i;
    return -1;
}

static void bytes_to_hex(const uint8_t *data, size_t len, char *out) {
    for (size_t i = 0; i < len; i++)
        sprintf(out + i * 2, "%02x", data[i]);
}

static int hex_to_bytes(const char *hex, uint8_t *out, size_t max) {
    size_t len = strlen(hex);
    if (len % 2 != 0) return -1;
    size_t nbytes = len / 2;
    if (nbytes > max) return -1;
    for (size_t i = 0; i < nbytes; i++) {
        unsigned int b;
        if (sscanf(hex + i * 2, "%2x", &b) != 1) return -1;
        out[i] = (uint8_t)b;
    }
    return (int)nbytes;
}

/* syllable_count no longer needed — using WLING_SYLLABLES() from wling.h */

/* get MAC address (Linux only, falls back to random) */
static int get_mac(uint8_t mac[6]) {
#ifdef __linux__
    int fd = socket(AF_INET, SOCK_DGRAM, 0);
    if (fd < 0) goto random_mac;
    struct ifreq ifr;
    /* try common interface names */
    const char *names[] = {"eth0", "enp0s1", "enp0s3", "wlan0", "ens3", NULL};
    for (int i = 0; names[i]; i++) {
        memset(&ifr, 0, sizeof(ifr));
        strncpy(ifr.ifr_name, names[i], IFNAMSIZ - 1);
        if (ioctl(fd, SIOCGIFHWADDR, &ifr) == 0) {
            memcpy(mac, ifr.ifr_hwaddr.sa_data, 6);
            close(fd);
            if (mac[0] || mac[1] || mac[2] || mac[3] || mac[4] || mac[5])
                return 0;
        }
    }
    close(fd);
random_mac:
#endif
    /* random multicast MAC (bit 0 of first byte set = multicast = "random") */
    read_entropy(mac, 6);
    mac[0] |= 0x01;
    return 0;
}

/* ---- UUID v1: timestamp + MAC ---- */

static void gen_v1(int nwords, const char *sep) {
    struct timeval tv;
    gettimeofday(&tv, NULL);

    /* UUID v1 timestamp: 100ns intervals since Oct 15, 1582 */
    uint64_t ts = ((uint64_t)tv.tv_sec + 12219292800ULL) * 10000000ULL +
                  (uint64_t)tv.tv_usec * 10;

    uint8_t uuid[18] = {0}; /* 16 bytes + 2 padding for extract */

    /* time_low (32 bits) */
    uuid[0] = (ts >> 24) & 0xFF;
    uuid[1] = (ts >> 16) & 0xFF;
    uuid[2] = (ts >> 8) & 0xFF;
    uuid[3] = ts & 0xFF;
    /* time_mid (16 bits) */
    uuid[4] = (ts >> 40) & 0xFF;
    uuid[5] = (ts >> 32) & 0xFF;
    /* time_hi_and_version (16 bits, version 1) */
    uuid[6] = ((ts >> 56) & 0x0F) | 0x10;
    uuid[7] = (ts >> 48) & 0xFF;
    /* clock_seq (14 bits random + variant) */
    uint8_t rnd[2];
    read_entropy(rnd, 2);
    uuid[8] = (rnd[0] & 0x3F) | 0x80; /* variant 10xx */
    uuid[9] = rnd[1];
    /* node (48 bits MAC) */
    uint8_t mac[6];
    get_mac(mac);
    memcpy(uuid + 10, mac, 6);

    emit_words(uuid, nwords, sep);
}

/* ---- UUID v4: pure random ---- */

static void gen_v4(int nwords, const char *sep) {
    size_t entropy_bytes = (nwords * WORDLIST_BITS + 7) / 8 + 2;
    uint8_t *entropy = calloc(1, entropy_bytes);
    read_entropy(entropy, entropy_bytes);
    emit_words(entropy, nwords, sep);
    free(entropy);
}

/* ---- UUID v5: SHA-1 of namespace + name ---- */

/* standard namespace UUIDs */
static const uint8_t NS_DNS[16] = {
    0x6b,0xa7,0xb8,0x10,0x9d,0xad,0x11,0xd1,
    0x80,0xb4,0x00,0xc0,0x4f,0xd4,0x30,0xc8
};
static const uint8_t NS_URL[16] = {
    0x6b,0xa7,0xb8,0x11,0x9d,0xad,0x11,0xd1,
    0x80,0xb4,0x00,0xc0,0x4f,0xd4,0x30,0xc8
};

static void gen_v5(const char *name, const char *ns_str, int nwords, const char *sep) {
    const uint8_t *ns = NS_DNS; /* default namespace */
    uint8_t custom_ns[16];

    if (ns_str) {
        if (strcmp(ns_str, "dns") == 0) ns = NS_DNS;
        else if (strcmp(ns_str, "url") == 0) ns = NS_URL;
        else {
            /* try to parse as hex UUID */
            char clean[33] = {0};
            int ci = 0;
            for (const char *p = ns_str; *p && ci < 32; p++)
                if (isxdigit(*p)) clean[ci++] = *p;
            if (ci == 32) {
                hex_to_bytes(clean, custom_ns, 16);
                ns = custom_ns;
            } else {
                /* hash the namespace string itself */
                uint8_t ns_hash[20];
                sha1((const uint8_t *)ns_str, strlen(ns_str), ns_hash);
                memcpy(custom_ns, ns_hash, 16);
                ns = custom_ns;
            }
        }
    }

    /* SHA-1(namespace || name) */
    size_t name_len = strlen(name);
    size_t total = 16 + name_len;
    uint8_t *input = malloc(total);
    memcpy(input, ns, 16);
    memcpy(input + 16, name, name_len);

    uint8_t hash[20];
    sha1(input, total, hash);
    free(input);

    /* set version (5) and variant (10xx) */
    hash[6] = (hash[6] & 0x0F) | 0x50;
    hash[8] = (hash[8] & 0x3F) | 0x80;

    /* add 2 bytes padding for extract_bits */
    uint8_t padded[22] = {0};
    memcpy(padded, hash, 20);

    emit_words(padded, nwords, sep);
}

/* ---- UUID v7: timestamp + random (sortable) ---- */

static void gen_v7(int nwords, const char *sep) {
    struct timeval tv;
    gettimeofday(&tv, NULL);
    uint64_t ts_ms = (uint64_t)tv.tv_sec * 1000 + tv.tv_usec / 1000;

    uint8_t uuid[18] = {0};

    /* 48-bit timestamp (ms since epoch) in first 6 bytes */
    uuid[0] = (ts_ms >> 40) & 0xFF;
    uuid[1] = (ts_ms >> 32) & 0xFF;
    uuid[2] = (ts_ms >> 24) & 0xFF;
    uuid[3] = (ts_ms >> 16) & 0xFF;
    uuid[4] = (ts_ms >> 8) & 0xFF;
    uuid[5] = ts_ms & 0xFF;

    /* version 7 in high nibble of byte 6 */
    uint8_t rnd[10];
    read_entropy(rnd, 10);
    uuid[6] = (rnd[0] & 0x0F) | 0x70;
    uuid[7] = rnd[1];

    /* variant 10xx */
    uuid[8] = (rnd[2] & 0x3F) | 0x80;
    memcpy(uuid + 9, rnd + 3, 7);

    emit_words(uuid, nwords, sep);
}

/* ---- WUUID w1: phrase (recursive context-free grammar) ---- */

/*
 * Generates grammatical templates for ANY word count using a recursive
 * English phrase structure grammar. Entropy bits select which production
 * rule to use at each branch point.
 *
 * Grammar (simplified English):
 *   S  → NP VP           (sentence = subject + predicate)
 *   NP → N | A N | A A N (noun phrase: optional adjectives + noun)
 *   VP → V | V NP | V PP | V NP PP  (verb phrase: verb + objects)
 *   PP → P NP            (prepositional phrase)
 *
 * For large N, S splits recursively into multiple clauses.
 * Template slots: A=adj, N=noun, V=verb, P=preposition
 */

/* Preposition indices populated lazily from BIP39 */
static int prep_indices[64];
static int prep_count = 0;

static void init_prepositions(void) {
    if (prep_count > 0) return;
    const char *preps[] = {
        "above", "across", "about", "around", "before", "behind", "below",
        "between", "beyond", "during", "inside", "into", "near", "off",
        "onto", "other", "outdoor", "outside", "over", "south", "there",
        "together", "toward", "under", "upon", "upper", "within", NULL
    };
    for (int i = 0; preps[i]; i++) {
        int idx = word_to_index(preps[i]);
        if (idx >= 0 && prep_count < 64)
            prep_indices[prep_count++] = idx;
    }
}

static uint16_t pick_from_cat(const uint16_t *cat_arr, size_t cat_len,
                              const uint8_t *entropy, int *bit_pos) {
    uint16_t raw = extract_bits(entropy, *bit_pos);
    *bit_pos += WORDLIST_BITS;
    return cat_arr[raw % cat_len];
}

static uint16_t pick_word_for_slot(char slot, const uint8_t *entropy, int *bit_pos) {
    switch (slot) {
    case 'A': return pick_from_cat(cat_adjs, CAT_ADJS_LEN, entropy, bit_pos);
    case 'V': return pick_from_cat(cat_verbs, CAT_VERBS_LEN, entropy, bit_pos);
    case 'P': {
        init_prepositions();
        uint16_t raw = extract_bits(entropy, *bit_pos);
        *bit_pos += WORDLIST_BITS;
        return prep_indices[raw % prep_count];
    }
    default:  return pick_from_cat(cat_nouns, CAT_NOUNS_LEN, entropy, bit_pos);
    }
}

/* choose between n options using entropy */
static int choose(int n, const uint8_t *entropy, int *bit_pos) {
    uint16_t raw = extract_bits(entropy, *bit_pos);
    *bit_pos += WORDLIST_BITS;
    return raw % n;
}

/* recursive template builder */
static void gen_np(char *buf, int *pos, int n,
                   const uint8_t *ent __attribute__((unused)),
                   int *bp __attribute__((unused))) {
    /* NP with exactly n words: (n-1) adjectives + 1 noun */
    for (int i = 0; i < n - 1; i++) buf[(*pos)++] = 'A';
    buf[(*pos)++] = 'N';
}

static void gen_pp(char *buf, int *pos, int n, const uint8_t *ent, int *bp) {
    /* PP = P + NP(n-1) */
    buf[(*pos)++] = 'P';
    gen_np(buf, pos, n - 1, ent, bp);
}

static void gen_vp(char *buf, int *pos, int n, const uint8_t *ent, int *bp) {
    if (n <= 0) return;
    if (n == 1) { buf[(*pos)++] = 'V'; return; }

    buf[(*pos)++] = 'V'; /* verb always first */
    int rest = n - 1;

    if (rest <= 2) {
        /* V + NP */
        gen_np(buf, pos, rest, ent, bp);
    } else {
        /* V + NP, or V + PP, or V + NP + PP */
        int choice = choose(3, ent, bp);
        if (choice == 0) {
            /* V + NP(rest) */
            gen_np(buf, pos, rest, ent, bp);
        } else if (choice == 1 && rest >= 2) {
            /* V + PP(rest) — "run through forest" */
            gen_pp(buf, pos, rest, ent, bp);
        } else if (rest >= 4) {
            /* V + NP(k) + PP(rest-k) — "throw ball across river" */
            int np_len = 1 + choose(rest - 3, ent, bp); /* at least 1 for NP, 2+ for PP */
            int pp_len = rest - np_len;
            if (pp_len < 2) { np_len = rest - 2; pp_len = 2; }
            gen_np(buf, pos, np_len, ent, bp);
            gen_pp(buf, pos, pp_len, ent, bp);
        } else {
            /* fallback: V + NP */
            gen_np(buf, pos, rest, ent, bp);
        }
    }
}

static void gen_clause(char *buf, int *pos, int n, const uint8_t *ent, int *bp) {
    if (n <= 0) return;
    if (n == 1) {
        /* single word: random POS */
        char opts[] = "NAV";
        buf[(*pos)++] = opts[choose(3, ent, bp)];
        return;
    }

    /* split into NP (subject) + VP (predicate) */
    int np_min = 1, np_max = (n <= 3) ? 1 : (n / 3); /* subject ~1/3 of clause */
    if (np_max > 3) np_max = 3; /* adj adj noun max */
    int np_len = np_min + choose(np_max - np_min + 1, ent, bp);
    int vp_len = n - np_len;
    if (vp_len < 1) { np_len = n - 1; vp_len = 1; }

    gen_np(buf, pos, np_len, ent, bp);
    gen_vp(buf, pos, vp_len, ent, bp);
}

static void gen_template(char *buf, int n, const uint8_t *ent, int *bp) {
    int pos = 0;

    if (n <= 8) {
        /* single clause */
        gen_clause(buf, &pos, n, ent, bp);
    } else {
        /* split into multiple clauses of 3-8 words */
        int remaining = n;
        while (remaining > 0) {
            int clause_len;
            if (remaining <= 8) {
                clause_len = remaining;
            } else if (remaining <= 11) {
                /* avoid leaving a 1-2 word runt */
                clause_len = remaining / 2 + choose(2, ent, bp);
                if (remaining - clause_len < 3) clause_len = remaining - 3;
                if (clause_len < 3) clause_len = 3;
            } else {
                clause_len = 3 + choose(6, ent, bp); /* 3-8 words */
            }
            gen_clause(buf, &pos, clause_len, ent, bp);
            remaining -= clause_len;
        }
    }
    buf[pos] = '\0';
}

static void gen_w1(int nwords, const char *sep) {
    /* entropy: grammar choices + word selections */
    size_t entropy_bytes = ((nwords + 10) * WORDLIST_BITS + 7) / 8 + 4;
    uint8_t *entropy = calloc(1, entropy_bytes);
    read_entropy(entropy, entropy_bytes);
    int bit_pos = 0;

    /* generate template from grammar */
    char tmpl[64];
    gen_template(tmpl, nwords, entropy, &bit_pos);

    /* fill slots */
    for (int w = 0; w < nwords; w++) {
        if (w > 0) fputs(sep, stdout);
        uint16_t idx = pick_word_for_slot(tmpl[w], entropy, &bit_pos);
        fputs(wordlist[idx], stdout);
    }
    putchar('\n');
    free(entropy);
}

/* ---- WUUID w2: name (hash any string → deterministic prefix + random) ---- */

static void gen_w2(const char *name, int nwords, const char *sep) {
    /* SHA-1 the namespace string to get deterministic bytes */
    uint8_t hash[20];
    sha1((const uint8_t *)name, strlen(name), hash);

    /* use hash bytes for first half of words, random for rest */
    int prefix_words = nwords / 2;
    if (prefix_words < 1) prefix_words = 1;
    if (prefix_words > nwords) prefix_words = nwords;

    /* pad hash for extract_bits */
    uint8_t padded[24] = {0};
    memcpy(padded, hash, 20);

    /* random suffix */
    int remaining = nwords - prefix_words;
    size_t rnd_bytes = (remaining * WORDLIST_BITS + 7) / 8 + 2;
    uint8_t *rnd = calloc(1, rnd_bytes);
    if (remaining > 0) read_entropy(rnd, rnd_bytes);

    for (int w = 0; w < nwords; w++) {
        if (w > 0) fputs(sep, stdout);
        if (w < prefix_words) {
            uint16_t idx = extract_bits(padded, w * WORDLIST_BITS);
            fputs(wordlist[idx], stdout);
        } else {
            uint16_t idx = extract_bits(rnd, (w - prefix_words) * WORDLIST_BITS);
            fputs(wordlist[idx], stdout);
        }
    }
    putchar('\n');
    free(rnd);
}

/* ---- WUUID w3: euphonic (phoneme-aware, prosodic, semantic) ---- */

/*
 * Scoring based on linguistic principles:
 *
 * PHONOTACTICS: consonant-ending → vowel-start (and vice versa) creates
 * smooth transitions. "bright ocean" flows; "bright crane" clunks.
 *
 * PROSODY: alternating syllable counts create rhythm.
 * "tiger-elaborate-fox-remember" has a trochaic/dactylic feel.
 *
 * SEMANTIC COHESION: words from the same or related semantic fields
 * are easier to remember as a unit. "ocean-wave-coral-deep" sticks;
 * "ocean-hammer-legal-verb" doesn't.
 *
 * VARIETY: no repeated first letters (alliteration is catchy but
 * confusable), no repeated words.
 */

static int euphony_score_ling(uint16_t a_idx, uint16_t b_idx) {
    int score = 0;
    const char *a = wordlist[a_idx];
    const char *b = wordlist[b_idx];

    /* same word = reject */
    if (a_idx == b_idx) return -100;

    /* PHONOTACTICS: C→V or V→C boundary = smooth (+4) */
    int a_last_v = WLING_LAST_V(a_idx);
    int b_first_v = WLING_FIRST_V(b_idx);
    if (a_last_v != b_first_v) score += 4; /* C→V or V→C: smooth */
    else score -= 1; /* C→C or V→V: slightly rough */

    /* PROSODY: alternating syllable count (+3) */
    int sa = WLING_SYLLABLES(a_idx), sb = WLING_SYLLABLES(b_idx);
    if ((sa == 1 && sb >= 3) || (sa >= 3 && sb == 1)) score += 3;
    else if (sa != sb) score += 1;
    /* same syllable count = monotonous, no bonus */

    /* VARIETY: different first letter (+2) */
    if (a[0] != b[0]) score += 2;

    /* VARIETY: different ending sound (+1) */
    size_t la = strlen(a), lb = strlen(b);
    if (la > 0 && lb > 0 && a[la-1] != b[lb-1]) score += 1;

    /* SEMANTIC COHESION: same non-abstract field = memorable (+2) */
    int fa = WLING_FIELD(a_idx), fb = WLING_FIELD(b_idx);
    if (fa == fb && fa != SF_ABSTRACT) score += 2;
    /* related fields (animal+nature, food+nature, tool+place) (+1) */
    else if ((fa == SF_ANIMAL && fb == SF_NATURE) ||
             (fa == SF_NATURE && fb == SF_ANIMAL) ||
             (fa == SF_FOOD && fb == SF_NATURE) ||
             (fa == SF_NATURE && fb == SF_FOOD) ||
             (fa == SF_TOOL && fb == SF_PLACE) ||
             (fa == SF_PLACE && fb == SF_TOOL))
        score += 1;

    return score;
}

static void gen_w3(int nwords, const char *sep) {
    /* 12 candidates per slot for good selection pressure */
    int candidates_per = 12;
    size_t entropy_bytes = (nwords * candidates_per * WORDLIST_BITS + 7) / 8 + 4;
    uint8_t *entropy = calloc(1, entropy_bytes);
    read_entropy(entropy, entropy_bytes);

    uint16_t chosen[24];
    int bit_pos = 0;

    /* pick first word: prefer concrete (non-abstract) words */
    {
        int best_score = -999;
        uint16_t best = 0;
        for (int c = 0; c < candidates_per; c++) {
            uint16_t raw = extract_bits(entropy, bit_pos);
            bit_pos += WORDLIST_BITS;
            uint16_t cand = raw % WORDLIST_SIZE;
            int score = (WLING_FIELD(cand) != SF_ABSTRACT) ? 3 : 0;
            score += (WLING_SYLLABLES(cand) <= 2) ? 1 : 0; /* prefer short opener */
            if (score > best_score) { best_score = score; best = cand; }
        }
        chosen[0] = best;
    }

    /* subsequent words: score against previous 1-2 words */
    for (int w = 1; w < nwords; w++) {
        int best_score = -999;
        uint16_t best = 0;
        for (int c = 0; c < candidates_per; c++) {
            uint16_t raw = extract_bits(entropy, bit_pos);
            bit_pos += WORDLIST_BITS;
            uint16_t cand = raw % WORDLIST_SIZE;
            int score = euphony_score_ling(chosen[w-1], cand);
            if (w >= 2) score += euphony_score_ling(chosen[w-2], cand) / 2;
            /* check for repeats with all previous */
            for (int p = 0; p < w; p++)
                if (chosen[p] == cand) score -= 50;
            if (score > best_score) { best_score = score; best = cand; }
        }
        chosen[w] = best;
    }

    for (int w = 0; w < nwords; w++) {
        if (w > 0) fputs(sep, stdout);
        fputs(wordlist[chosen[w]], stdout);
    }
    putchar('\n');
    free(entropy);
}

/* ---- decode / encode ---- */

static int decode_words(const char *input, const char *sep) {
    char *buf = strdup(input);
    int nwords = 0;
    uint16_t indices[24];
    char *tok = strtok(buf, sep);
    while (tok && nwords < 24) {
        for (char *p = tok; *p; p++)
            if (*p >= 'A' && *p <= 'Z') *p += 32;
        int idx = word_to_index(tok);
        if (idx < 0) {
            fprintf(stderr, "wuuid: unknown word '%s'\n", tok);
            free(buf);
            return 1;
        }
        indices[nwords++] = (uint16_t)idx;
        tok = strtok(NULL, sep);
    }
    free(buf);

    size_t total_bits = nwords * WORDLIST_BITS;
    size_t nbytes = (total_bits + 7) / 8 + 2;
    uint8_t *data = calloc(1, nbytes);
    for (int i = 0; i < nwords; i++)
        set_bits(data, i * WORDLIST_BITS, indices[i], WORDLIST_BITS);

    size_t out_bytes = (total_bits + 7) / 8;
    char *hex = malloc(out_bytes * 2 + 1);
    bytes_to_hex(data, out_bytes, hex);
    hex[out_bytes * 2] = '\0';
    puts(hex);

    free(hex);
    free(data);
    return 0;
}

static int encode_hex(const char *hex, int nwords, const char *sep) {
    uint8_t data[64] = {0};
    int nbytes = hex_to_bytes(hex, data, sizeof(data) - 2);
    if (nbytes < 0) {
        fprintf(stderr, "wuuid: invalid hex\n");
        return 1;
    }
    if (nwords <= 0) {
        nwords = (nbytes * 8) / WORDLIST_BITS;
        if (nwords < 1) nwords = 1;
    }
    emit_words(data, nwords, sep);
    return 0;
}

/* ---- main ---- */

static void usage(const char *prog) {
    fprintf(stderr,
        "Usage: %s [version] [options]\n"
        "\n"
        "UUID versions (BIP39-encoded):\n"
        "  v1          timestamp + MAC address\n"
        "  v4          pure random (default)\n"
        "  v5 NAME     SHA-1 hash (deterministic)\n"
        "  v7          timestamp + random (sortable)\n"
        "\n"
        "WUUID versions (linguistically structured):\n"
        "  w1          phrase (grammatical templates)\n"
        "  w2 NAME     name (hash string → deterministic prefix + random)\n"
        "  w3          euphonic (phoneme-aware, prosodic, semantic)\n"
        "\n"
        "Options:\n"
        "  -n N        words per ID (default: 4, max: 24)\n"
        "  -c N        number of IDs to generate (default: 1)\n"
        "  -s SEP      separator (default: -)\n"
        "  -N NS       namespace for v5 (dns, url, or hex/string)\n"
        "  -d          decode: words → hex\n"
        "  -e HEX      encode: hex → words\n"
        "  -h          show this help\n"
        "\n"
        "Examples:\n"
        "  %s                        # 4 random words\n"
        "  %s v7 -n 6                # 6-word sortable ID\n"
        "  %s v5 user@example.com    # deterministic from email\n"
        "  %s w1 -n 6 -c 5           # grammatical phrases\n"
        "  %s w2 my-cool-project     # namespaced: hash prefix + random\n"
        "  %s w3 -c 5                # phonetically pleasant words\n",
        prog, prog, prog, prog, prog, prog, prog);
}

int main(int argc, char **argv) {
    int nwords = 4;
    int count = 1;
    const char *sep = "-";
    const char *ns = NULL;
    int decode = 0;
    const char *encode_str = NULL;
    const char *version = "v4";
    const char *v5_name = NULL;
    const char *w2_name = NULL;

    /* check for version subcommand */
    int arg_start = 1;
    if (argc > 1 && argv[1][0] != '-') {
        if (strcmp(argv[1], "v1") == 0 || strcmp(argv[1], "v4") == 0 ||
            strcmp(argv[1], "v5") == 0 || strcmp(argv[1], "v7") == 0 ||
            strcmp(argv[1], "w1") == 0 || strcmp(argv[1], "w2") == 0 ||
            strcmp(argv[1], "w3") == 0) {
            version = argv[1];
            arg_start = 2;

            /* v5 and w2 take a required argument */
            if (strcmp(version, "v5") == 0 && argc > 2 && argv[2][0] != '-') {
                v5_name = argv[2];
                arg_start = 3;
            }
            if (strcmp(version, "w2") == 0 && argc > 2 && argv[2][0] != '-') {
                w2_name = argv[2];
                arg_start = 3;
            }
        }
    }

    /* parse options starting after version args */
    optind = arg_start;
    int opt;
    while ((opt = getopt(argc, argv, "n:c:s:N:de:h")) != -1) {
        switch (opt) {
        case 'n': nwords = atoi(optarg); break;
        case 'c': count = atoi(optarg); break;
        case 's': sep = optarg; break;
        case 'N': ns = optarg; break;
        case 'd': decode = 1; break;
        case 'e': encode_str = optarg; break;
        case 'h': usage(argv[0]); return 0;
        default:  usage(argv[0]); return 1;
        }
    }

    /* encode mode */
    if (encode_str) return encode_hex(encode_str, nwords, sep);

    /* decode mode */
    if (decode) {
        if (optind < argc) {
            char buf[1024] = {0};
            for (int i = optind; i < argc; i++) {
                if (i > optind) strcat(buf, sep);
                strncat(buf, argv[i], sizeof(buf) - strlen(buf) - 2);
            }
            return decode_words(buf, sep);
        } else {
            char line[1024];
            while (fgets(line, sizeof(line), stdin)) {
                line[strcspn(line, "\n")] = '\0';
                if (line[0] && decode_words(line, sep) != 0) return 1;
            }
            return 0;
        }
    }

    /* validation */
    if (nwords < 1 || nwords > 24) {
        fprintf(stderr, "wuuid: words must be 1-24\n");
        return 1;
    }

    if (strcmp(version, "v5") == 0 && !v5_name) {
        fprintf(stderr, "wuuid: v5 requires a name argument\n");
        usage(argv[0]);
        return 1;
    }
    if (strcmp(version, "w2") == 0 && !w2_name) {
        fprintf(stderr, "wuuid: w2 requires a name argument\n");
        usage(argv[0]);
        return 1;
    }

    /* generate */
    for (int i = 0; i < count; i++) {
        if (strcmp(version, "v1") == 0)      gen_v1(nwords, sep);
        else if (strcmp(version, "v4") == 0)  gen_v4(nwords, sep);
        else if (strcmp(version, "v5") == 0)  gen_v5(v5_name, ns, nwords, sep);
        else if (strcmp(version, "v7") == 0)  gen_v7(nwords, sep);
        else if (strcmp(version, "w1") == 0)  gen_w1(nwords, sep);
        else if (strcmp(version, "w2") == 0)  gen_w2(w2_name, nwords, sep);
        else if (strcmp(version, "w3") == 0)  gen_w3(nwords, sep);
        else {
            fprintf(stderr, "wuuid: unknown version '%s'\n", version);
            return 1;
        }
    }

    return 0;
}
