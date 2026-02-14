// Expected exit code: 5
int main(void) {
    int a = 0;
    while (a < 100) {
        if (a == 5)
            break;
        a = a + 1;
    }
    return a;
}
