// Expected exit code: 3
int counter(void) {
    static long c = 0L;
    c = c + 1L;
    return c;
}

int main(void) {
    counter();
    counter();
    return counter();
}
