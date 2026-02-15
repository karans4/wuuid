# wuuid

UUID but words.

```
$ wuuid
noble-midnight-raise-immense

$ wuuid v7 -n 6
account-title-movie-make-wave-chaos

$ wuuid w1 -n 8 -c 3
traffic-banana-act-episode-during-liquid-busy-powder
tiger-fashion-outside-olive-festival-heavy-dynamic-pond
heavy-fever-deny-inside-local-early-dad

$ wuuid w3 -c 3
turtle-buffalo-moon-hawk
ocean-actor-assume-guard
lava-sure-cricket-open
```

Single C file, zero dependencies, BIP39 wordlist (2048 words, 11 bits each). All word IDs round-trip through hex.

## Versions

### UUID versions (BIP39-encoded)

Standard UUID layouts encoded as words instead of hex.

| Version | What | Example |
|---------|------|---------|
| `v1` | timestamp + MAC | IDs from same machine share suffix |
| `v4` | pure random (default) | `wuuid` or `wuuid v4` |
| `v5 NAME` | SHA-1 hash | same input always gives same output |
| `v7` | timestamp + random | IDs from same moment share prefix, sortable |

### WUUID versions (linguistically structured)

Word-native formats that use linguistic structure for memorability.

| Version | What | Example |
|---------|------|---------|
| `w1` | **phrase** — recursive context-free grammar | `similar-kiwi-convince-below-general-bird` |
| `w2 NAME` | **name** — hash any string to deterministic prefix | `fossil-card-{random}-{random}` |
| `w3` | **euphonic** — phoneme/prosody/semantic scoring | `turtle-buffalo-moon-hawk` |

**w1** generates grammatical English phrase structure (NP + VP clauses) for any word count 1-24. The grammar is recursive: large counts split into multiple clauses. Entropy bits select which production rules fire at each branch, so you get varied structures like A-N-V-P-A-N, N-V-N, A-A-N-V-P-N, etc.

**w2** hashes any arbitrary string (project names, emails, URLs, whatever) to a deterministic BIP39 prefix, then fills the rest with random words. Useful for namespacing.

**w3** scores 12 random candidates per word slot on four axes:
- **Phonotactics**: consonant-ending words prefer vowel-starting neighbors (and vice versa)
- **Prosody**: alternating syllable counts create natural rhythm
- **Semantic cohesion**: words from the same field (animals, nature, food, tools) cluster together
- **Variety**: no repeated first letters, no duplicate words

## Options

```
-n N     words per ID (default: 4, max: 24)
-c N     number of IDs (default: 1)
-s SEP   separator (default: -)
-N NS    namespace for v5 (dns, url, or any string)
-d       decode words to hex
-e HEX   encode hex to words
```

## Round-trip

All versions output BIP39 words, so encode/decode works universally:

```
$ ID=$(wuuid w1 -n 6)
$ echo $ID
direct-fresh-old-number-brush-fan

$ echo $ID | wuuid -d
3e8b9a684bb1d2a580

$ wuuid -e 3e8b9a684bb1d2a580 -n 6
direct-fresh-old-number-brush-fan
```

## Build

```
gcc -O2 -o wuuid wuuid.c
cp wuuid ~/.local/bin/
```

