int *malloc(int);

int main() {
    int *p;
    p = malloc(4 * 10);
    *p = 454932943;
    *(p + 1) = 19457513;
    return (*p + *(p + 1)) / 4589459;
}
