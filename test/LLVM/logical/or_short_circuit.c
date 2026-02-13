// Expected exit code: 1
int main(void) {
    int a = 0 || 1;
    int b = 0 || 0;
    return a + b;
}
