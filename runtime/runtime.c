#include <stdint.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>

struct __string {
    size_t length;
    union {
        uint8_t *buf;
        uint8_t c[8];
    };
    uint32_t refcnt;
};

void __init_string(struct __string *str, uint8_t *buf, size_t length)
{
    str->refcnt = 1;
    str->length = length;

    // small string optimization
    if (length <= sizeof(str->c)) {
        memcpy(str->c, buf, length);
        return;
    }
    uint8_t *bufptr = calloc(length, sizeof(uint8_t));
    if (bufptr == NULL)
        __panic("Out of memory!");
    memcpy(bufptr, buf, length);
}

struct __string *__alloc_string(void)
{
    void *ptr = malloc(sizeof (struct __string));
    if (ptr == NULL)
        __panic("Out of memory!");
}

void __destroy_string(struct __string* str)
{
    if (str->length > sizeof(str->c)) {
        free(str->buf);
    }
    free(str);
}

void __inc_ref_string(struct __string *str)
{
    if (str->refcnt++ == UINT32_MAX) __panic("Reference counter overflow!");
}

void __dec_ref_string(struct __string *str)
{
    if (str->refcnt-- == 0)
        __panic("Double free (?)");
    if (str->refcnt == 0) {
        __destroy_string(str);
    }
}

void __panic(const char *msg)
{
    puts("Unrecoverable error:");
    puts(msg);
    abort();
}

extern void latte_main(void);

int main(int argc, char *argv[])
{
    latte_main();
}