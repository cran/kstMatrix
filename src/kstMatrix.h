#if LONG_MAX == 2147483647L
#define BITS_PER_LONG        32
#define LD_BPL                5
#define BPL_M1               31
#else
#define BITS_PER_LONG        64
#define LD_BPL                6
#define BPL_M1               63
#endif


#define word_no(i)           ((i+BPL_M1)>>LD_BPL)
#define word_cnt(i)          (i>>LD_BPL)
#define word_pos(i)          (i%BITS_PER_LONG)

typedef long *tstate;

#ifndef helpers
extern int non_empty(tstate s, int w_size);
extern void init_bit_count(void);
extern int set_size(tstate vector);
extern int set_dist(long *set1, long *set2, int words);
extern int subset(long *a, long *b, long l);
extern int equal_set(long *a, long *b, long l);
extern void section_set(long *dest, long *src1, long *src2, long  l);
extern void union_set(long *dest, long *src1, long *src2, long  l);
extern void diff_set(long *dest, long *src1, long *src2, long  l);
extern long *import_famset(int noi, int nos, int *data );
extern void print_matrix(tstate x, long rows);


extern long size_b, size_q, size_s, size_w;
extern int bit_count[256];
extern long *temp;
#endif
