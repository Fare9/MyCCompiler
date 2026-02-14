// Expected exit code: 5
int main(void) {
    int a = 5;
    int b = ++a;
    int c = a++;
    return b + c - a;
}
