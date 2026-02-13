// Expected exit code: 101
int main(void) {
    int a = 0;
    int i;
    for (i = -100; i <= 0; i = i + 1)
        a = a + 1;
    return a;
}
