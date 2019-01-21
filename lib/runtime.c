#include <stdint.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <stdbool.h>
#include <sys/types.h>
#include <signal.h>

struct __array {
    int32_t length;
    void *buffer;
};

struct __string {
    size_t length;
    union {
        uint8_t *buf;
        uint8_t c[8];
    };
    uint32_t refcnt;
};

void __panic(const char *msg)
{
    puts("Unrecoverable error:");
    puts(msg);
    abort();
}

struct __array *__alloc_array(int32_t nitems, int32_t el_size)
{
    struct __array *arr = malloc(sizeof (struct __array));
    if (arr == NULL) {
        __panic("Out of memory");
    }
    arr->length = nitems;

    arr->buffer = calloc(nitems, el_size);
    if (arr->buffer == NULL) {
        __panic("Out of memory");
    }

    return arr;
}

void *__get_array_buffer(struct __array *arr)
{
    return arr->buffer;
}

int32_t __get_array_length(struct __array *arr)
{
    return arr->length;
}

void __init_string(struct __string *str, uint8_t *buf, size_t length, bool move_buffer)
{
    str->refcnt = 1;
    str->length = length;

    // small string optimization
    if (length <= sizeof(str->c)) {
        memcpy(str->c, buf, length);
        return;
    }
    uint8_t *bufptr;
    if (move_buffer) {
        bufptr = buf;
    } else {
        bufptr = calloc(length, sizeof(uint8_t));
        memcpy(bufptr, buf, length);
    }
    if (bufptr == NULL)
        __panic("Out of memory!");
    str->buf = bufptr;
}


struct __string *__alloc_string(void)
{
    struct __string *ptr = malloc(sizeof (struct __string));
    if (ptr == NULL)
        __panic("Out of memory!");
    return ptr;
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

uint8_t* __get_buf_string(struct __string *str)
{
    if (str->length <= sizeof(str->c)) {
        return str->c;
    }
    return str->buf;
}

struct __string* __concat_strings(struct __string *strA, struct __string *strB)
{
    struct __string* newString = __alloc_string();
    size_t size = strA->length + strB->length;
    uint8_t *buf = calloc(size, 1);
    memcpy(buf, __get_buf_string(strA), strA->length);
    memcpy(buf + strA->length, __get_buf_string(strB), strB->length);
    __init_string(newString, buf, size, true);
    return newString;
}

void __latte_std_printInt(int32_t n)
{
    fprintf(stdout, "%d\n", n);
}

void __latte_std_printString(struct __string *str)
{
    fwrite(__get_buf_string(str), sizeof(uint8_t), str->length, stdout);
    fprintf(stdout, "\n");
}

void __latte_std_error(void)
{
    fputs("runtime error", stderr);
    exit(1);
}

int32_t __latte_std_readInt(void)
{
    int32_t n;
    fscanf(stdin, "%d", &n);
    return n;
}

struct __string* __latte_std_readString(void)
{
    uint8_t tmp[1024];
    /// TODO: fix this
    fscanf(stdin, "%s", (char *)&tmp);
    size_t len = strlen((char*)tmp);
    struct __string *str = __alloc_string();
    __init_string(str, tmp, len, false);
    return str;
}


void fpe_handler(int signo, siginfo_t *siginfo, void *data)
{
    switch (signo) {
        case SIGFPE:
            __panic("Division by zero!");
        default:
            __panic("Unknown signal received!");
    }
}

extern void latte_main(void);

int main(int argc, char *argv[])
{
    struct sigaction sa;
    sa.sa_sigaction = fpe_handler;
    sigemptyset(&sa.sa_mask);
    sa.sa_flags = 0;
    if (sigaction(SIGFPE, &sa, NULL) != 0) {
        __panic("Unable to setup error handler!");
    }

    latte_main();
    return 0;
}
