// Expected exit code: 5
int main(void) {
    int a = 0;
    for (;;) {
        a = a + 1;
        if (a == 5)
            break;
    }
    return a;
}
