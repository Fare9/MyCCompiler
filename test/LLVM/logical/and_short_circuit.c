// Expected exit code: 1
int main(void) {
    int a = 1 && 2;
    int b = 0 && 5;
    return a + b;
}
