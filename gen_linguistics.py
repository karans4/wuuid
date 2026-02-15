#!/usr/bin/env python3
"""Generate wling.h — packed linguistic metadata for BIP39 wordlist."""

import re, sys

# --- Parse wordlist.h ---
def parse_wordlist(path):
    words = []
    with open(path) as f:
        for line in f:
            m = re.search(r'"([a-z]+)"', line)
            if m:
                words.append(m.group(1))
    return words

# --- Syllable count (vowel group method) ---
def syllable_count(word):
    vowels = set('aeiou')
    count = 0
    prev_vowel = False
    for i, ch in enumerate(word):
        is_v = ch in vowels or (ch == 'y' and i > 0)
        if is_v and not prev_vowel:
            count += 1
        prev_vowel = is_v
    # Silent e: trailing 'e' not part of 'le' digraph reduces count
    if word.endswith('e') and not word.endswith('le') and count > 1:
        count -= 1
    return max(count, 1)

# --- Phoneme classes ---
def first_vowel(word):
    return word[0] in 'aeiou'

def last_vowel(word):
    return word[-1] in 'aeiouy'

# --- Stress hint ---
FINAL_STRESS_SUFFIXES = ('tion', 'ment', 'ness', 'ize', 'ate', 'ect', 'ude', 'rupt',
                         'ise', 'ade', 'ene', 'ese', 'ique', 'ette', 'oon', 'eer',
                         'ign', 'ose', 'ute', 'ure')

def stress_hint(word):
    sc = syllable_count(word)
    if sc <= 1:
        return 0  # mono = initial
    for suf in FINAL_STRESS_SUFFIXES:
        if word.endswith(suf):
            return 1  # final stress
    return 0  # default initial

# --- Semantic fields ---
ANIMALS = set("""
cat dog fish tiger lion wolf fox hawk eagle deer bear goat horse duck swan whale
shark snake lizard frog crab shrimp oyster lobster salmon tuna pigeon parrot robin
crane dove hen rooster pony mule donkey camel elephant monkey gorilla panda koala
rabbit hamster mouse rat squirrel raccoon skunk turtle tortoise gecko spider ant bee
wasp beetle cricket dragonfly butterfly moth worm snail slug leopard panther jaguar
cheetah buffalo bison hippo giraffe zebra kangaroo penguin flamingo pelican ostrich turkey
""".split())

NATURE = set("""
ocean river lake mountain forest desert valley island cliff cave reef beach meadow
field garden pond creek swamp volcano glacier canyon jungle prairie tundra storm rain
snow ice frost fog cloud sun moon star sky wind thunder lightning fire flame smoke
wave tide flood drought seed tree flower leaf root branch vine grass moss fern
mushroom coral stone rock crystal sand dust mud soil earth lava gem diamond gold
silver copper iron steel bronze marble wood timber oak maple palm bamboo pine cedar
willow rose lily tulip daisy orchid sunflower
""".split())

BODY = set("""
arm leg hand foot head eye ear nose mouth tooth tongue lip neck chest heart lung
brain bone skin blood muscle nerve hair finger thumb knee elbow shoulder hip spine
skull rib jaw wrist ankle palm nail belly
""".split())

FOOD = set("""
apple bread butter cake candy cheese cherry chicken chocolate coffee cookie corn cream
diet egg fat fish fruit grape honey ice juice lemon lunch meal meat milk mushroom nut
olive onion orange pasta peach peanut pepper pizza plum potato rice salad salt sauce
soup spice steak sugar tea tomato tuna vanilla walnut wheat wine
""".split())

TOOL = set("""
blade bolt brick brush bucket cable cage chain clip drill engine fabric fence flag
frame gear glass glove hammer handle helmet hook key knife ladder lamp lens lever lock
magnet mask mirror nail needle net paddle panel patch pen pipe plate plug pump rack
razor ribbon ring rod rope saddle saw shield slot socket spike stamp stick string sword
tape tent tool torch tower trap tube valve wheel wire
""".split())

PLACE = set("""
arena bar barn base beach bench booth bridge cabin camp castle cellar church city
clinic club corner court den door exit farm floor fort gallery garage gate gym hall
harbor home hospital hotel house hub inn jail kitchen lab library lobby lounge mall
market museum office palace park path plaza pool port prison pub ranch road roof room
salon school shelter shop shrine square stadium stage station store street studio temple
theater tower tunnel vault village wall ward zoo
""".split())

# Priority: ANIMAL > NATURE > BODY > FOOD > TOOL > PLACE > ABSTRACT
def semantic_field(word):
    if word in ANIMALS: return 0
    if word in NATURE:  return 1
    if word in BODY:    return 2
    if word in FOOD:    return 3
    if word in TOOL:    return 4
    if word in PLACE:   return 5
    return 6  # ABSTRACT

# --- Pack byte ---
def pack_byte(word):
    sf = semantic_field(word)
    sc = syllable_count(word)
    sc_bits = min(sc - 1, 3)  # 0=1syl, 1=2syl, 2=3syl, 3=4+
    fv = 1 if first_vowel(word) else 0
    lv = 1 if last_vowel(word) else 0
    st = stress_hint(word)
    return (sf & 0x07) | ((sc_bits & 0x03) << 3) | ((fv & 1) << 5) | ((lv & 1) << 6) | ((st & 1) << 7)

# --- Main ---
def main():
    words = parse_wordlist('/home/k-with-claude/wuuid/wordlist.h')
    assert len(words) == 2048, f"Expected 2048 words, got {len(words)}"

    packed = [pack_byte(w) for w in words]

    # Generate header
    lines = []
    lines.append('#ifndef WLING_H')
    lines.append('#define WLING_H')
    lines.append('#include <stdint.h>')
    lines.append('')
    lines.append('/* Per-word linguistic data, packed into one byte:')
    lines.append(' * bits 0-2: semantic field (0-6)')
    lines.append(' * bits 3-4: syllable count (0=1syl, 1=2syl, 2=3syl, 3=4+syl)')
    lines.append(' * bit 5: first phoneme (0=consonant, 1=vowel)')
    lines.append(' * bit 6: last phoneme (0=consonant, 1=vowel)')
    lines.append(' * bit 7: stress (0=initial/mono, 1=final)')
    lines.append(' */')
    lines.append('static const uint8_t word_ling[2048] = {')

    for i in range(0, 2048, 16):
        chunk = packed[i:i+16]
        hex_vals = ', '.join(f'0x{b:02x}' for b in chunk)
        comment = f'/* {i:4d}: {words[i]}..{words[min(i+15, 2047)]} */'
        lines.append(f'    {hex_vals}, {comment}')

    lines.append('};')
    lines.append('')
    lines.append('/* Accessor macros */')
    lines.append('#define WLING_FIELD(i)    (word_ling[i] & 0x07)')
    lines.append('#define WLING_SYLLABLES(i) (((word_ling[i] >> 3) & 0x03) + 1)')
    lines.append('#define WLING_FIRST_V(i)  ((word_ling[i] >> 5) & 1)')
    lines.append('#define WLING_LAST_V(i)   ((word_ling[i] >> 6) & 1)')
    lines.append('#define WLING_STRESS(i)   ((word_ling[i] >> 7) & 1)')
    lines.append('')
    lines.append('/* Semantic field names */')
    lines.append('#define SF_ANIMAL  0')
    lines.append('#define SF_NATURE  1')
    lines.append('#define SF_BODY    2')
    lines.append('#define SF_FOOD    3')
    lines.append('#define SF_TOOL    4')
    lines.append('#define SF_PLACE   5')
    lines.append('#define SF_ABSTRACT 6')
    lines.append('')
    lines.append('#endif')

    with open('/home/k-with-claude/wuuid/wling.h', 'w') as f:
        f.write('\n'.join(lines) + '\n')

    # --- Summary ---
    field_names = ['ANIMAL', 'NATURE', 'BODY', 'FOOD', 'TOOL', 'PLACE', 'ABSTRACT']
    field_counts = [0] * 7
    syl_counts = [0] * 5  # index 1-4
    first_v = 0
    first_c = 0

    for i, w in enumerate(words):
        sf = semantic_field(w)
        field_counts[sf] += 1
        sc = min(syllable_count(w), 4)
        syl_counts[sc] += 1
        if first_vowel(w):
            first_v += 1
        else:
            first_c += 1

    print("=== Semantic Field Distribution ===")
    for i, name in enumerate(field_names):
        print(f"  {name:10s}: {field_counts[i]:4d}")
    print(f"  {'TOTAL':10s}: {sum(field_counts):4d}")

    print("\n=== Syllable Distribution ===")
    for s in range(1, 5):
        label = f"{s}{'+'if s==4 else ''} syl"
        print(f"  {label:6s}: {syl_counts[s]:4d}")

    print(f"\n=== First Phoneme ===")
    print(f"  Vowel:     {first_v:4d}")
    print(f"  Consonant: {first_c:4d}")

    # --- Verification ---
    print("\n=== Verification ===")
    test_words = {'tiger': (0, 2, False, False),
                  'ocean': (1, 2, True, False),
                  'arm':   (2, 1, True, False),
                  'apple': (3, 2, True, True),
                  'blade': (4, 1, False, True),
                  'arena': (5, 3, True, True),
                  'above': (6, 2, True, True)}

    for tw, (exp_sf, exp_sc, exp_fv, exp_lv) in test_words.items():
        if tw in words:
            idx = words.index(tw)
            b = packed[idx]
            sf = b & 0x07
            sc = ((b >> 3) & 0x03) + 1
            fv = bool((b >> 5) & 1)
            lv = bool((b >> 6) & 1)
            st = (b >> 7) & 1
            ok = (sf == exp_sf and sc == exp_sc and fv == exp_fv and lv == exp_lv)
            status = "OK" if ok else "FAIL"
            print(f"  {tw:10s}: field={field_names[sf]:8s} syl={sc} first_v={fv} last_v={lv} stress={st} [{status}]")
            if not ok:
                print(f"    EXPECTED: field={field_names[exp_sf]:8s} syl={exp_sc} first_v={exp_fv} last_v={exp_lv}")
        else:
            print(f"  {tw:10s}: NOT IN WORDLIST")

    print("\nGenerated wling.h successfully.")

if __name__ == '__main__':
    main()
