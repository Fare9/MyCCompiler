// Expected exit code: 50
int main(void) {
    int a = 0;
    int i = 0;
    while (i < 10) {
        i = i + 1;
        if (i % 2 == 0)
            continue;
        a = a + 10;
    }
    return a;
}
