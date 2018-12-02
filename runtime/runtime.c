#include <stdint.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <stdbool.h>
#include <sys/types.h>
#include <signal.h>

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
    }
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

uint8_t* __get_buf_string(struct __string *str)
{
    if (str->length <= sizeof(str->c)) {
        return str->c;
    }
    return str->buf;
}

void __latte_std_printInt(int32_t n)
{
    fprintf(stdout, "%d", n);
}

void __latte_std_printString(struct __string *str)
{
    fwrite(__get_buf_string(str), sizeof(uint8_t), str->length, stdout);
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
    size_t r = fread(tmp, sizeof(uint8_t), 1024, stdin);
    struct __string *str = __alloc_string();
    __init_string(str, tmp, r, false);
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