#ifndef SHA1_H
#define SHA1_H

#include <stdint.h>
#include <string.h>

static inline uint32_t sha1_rol(uint32_t x, int n) {
    return (x << n) | (x >> (32 - n));
}

static void sha1(const uint8_t *data, size_t len, uint8_t hash[20]) {
    uint32_t h0 = 0x67452301;
    uint32_t h1 = 0xefcdab89;
    uint32_t h2 = 0x98badcfe;
    uint32_t h3 = 0x10325476;
    uint32_t h4 = 0xc3d2e1f0;

    size_t ml = len * 8;
    size_t mdi = len % 64;
    size_t plen = (mdi < 56) ? (56 - mdi) : (120 - mdi);

    uint8_t padded[len + plen + 8];
    memcpy(padded, data, len);
    padded[len] = 0x80;
    memset(padded + len + 1, 0, plen - 1);

    for (int i = 0; i < 8; i++) {
        padded[len + plen + 7 - i] = (ml >> (i * 8)) & 0xff;
    }

    for (size_t offset = 0; offset < len + plen + 8; offset += 64) {
        uint32_t w[80];
        for (int i = 0; i < 16; i++) {
            w[i] = ((uint32_t)padded[offset + i * 4] << 24) |
                   ((uint32_t)padded[offset + i * 4 + 1] << 16) |
                   ((uint32_t)padded[offset + i * 4 + 2] << 8) |
                   ((uint32_t)padded[offset + i * 4 + 3]);
        }
        for (int i = 16; i < 80; i++) {
            w[i] = sha1_rol(w[i - 3] ^ w[i - 8] ^ w[i - 14] ^ w[i - 16], 1);
        }

        uint32_t a = h0, b = h1, c = h2, d = h3, e = h4;

        for (int i = 0; i < 80; i++) {
            uint32_t f, k;
            if (i < 20) {
                f = (b & c) | ((~b) & d);
                k = 0x5a827999;
            } else if (i < 40) {
                f = b ^ c ^ d;
                k = 0x6ed9eba1;
            } else if (i < 60) {
                f = (b & c) | (b & d) | (c & d);
                k = 0x8f1bbcdc;
            } else {
                f = b ^ c ^ d;
                k = 0xca62c1d6;
            }

            uint32_t temp = sha1_rol(a, 5) + f + e + k + w[i];
            e = d;
            d = c;
            c = sha1_rol(b, 30);
            b = a;
            a = temp;
        }

        h0 += a;
        h1 += b;
        h2 += c;
        h3 += d;
        h4 += e;
    }

    for (int i = 0; i < 4; i++) {
        hash[i] = (h0 >> (24 - i * 8)) & 0xff;
        hash[4 + i] = (h1 >> (24 - i * 8)) & 0xff;
        hash[8 + i] = (h2 >> (24 - i * 8)) & 0xff;
        hash[12 + i] = (h3 >> (24 - i * 8)) & 0xff;
        hash[16 + i] = (h4 >> (24 - i * 8)) & 0xff;
    }
}

#endif
