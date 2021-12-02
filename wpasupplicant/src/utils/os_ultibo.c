#include "os_ultibo.h"
#include <sys/time.h>
#include <unistd.h>
#include "common.h"
#include "wpa_debug.h"

int os_get_reltime(struct os_reltime *t)
{
	// the ultibo implementation of this is currently to return the gettickcount64() value
	// as seconds and milliseconds. We don't have microsecond capability.
	long unixepoch;
	int msec;
	UltiboMonotonicTimeProc(&unixepoch, &msec);
	/* we don't get true microseconds here - may have to improve later - resolution only milliseconds */
	t->sec = unixepoch;
	t->usec = msec * 1000;

	return 0;
}

void * os_zalloc(size_t size)
{
	void *p = malloc(size);
	memset(p, 0, size);
	return p;
}

int os_daemonize(const char *pid_file)
{
	printf("os_daemonize called\n");

	return -1;
}

void os_daemonize_terminate(const char *pid_file)
{
	printf("os_deamonize_terminate called\n");

}

unsigned long os_random(void)
{
	return rand();
}

char * os_rel2abs_path(const char *rel_path)
{
	if (rel_path)
	  return strdup(rel_path); /* strdup(rel_path) can be used here */
	else
	  return NULL;
}

size_t os_strlcpy(char *dest, const char *src, size_t size)
{
	return strlcpy(dest, src, size);
}

void * os_memdup(const void *src, size_t n)
{
	void *r = malloc(n);

	if (r && src)
		os_memcpy(r, src, n);
	return r;
}

int os_get_random(unsigned char *buf, size_t len)
{
	for (size_t i = 0; i < len; i++)
	  buf[i] = (char)os_random();
	return 0;
}

int os_get_time(struct os_time *t)
{
	long unixepoch;
	int msec;
	UltiboTimeProc(&unixepoch, &msec);
	/* we don't get true microseconds here - may have to improve later */
	/* will need to out how to get the ultibo system to give microseconds */
	t->sec = unixepoch;
	t->usec = msec * 1000;
	return 0;
}

char * os_readfile(const char *name, size_t *len)
{
	printf("os_readfile called\n");
	return NULL;
}

int os_memcmp_const(const void *a, const void *b, size_t len)
{
	return memcmp(a, b, len);
}

void os_sleep(os_time_t sec, os_time_t usec)
{
	if (sec)
		sleep(sec);
	if (usec)
		usleep(usec);
}

int os_fdatasync(FILE *stream)
{
	return 0;
}

