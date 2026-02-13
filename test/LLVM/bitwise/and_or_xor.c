// Expected exit code: 7
int main(void) {
    int a = 15 & 7;
    int b = 5 | 2;
    int c = 15 ^ 8;
    return a + b - c;
}
