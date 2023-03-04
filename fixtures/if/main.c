int main() {
    int result;
    result = 1;
    if (1) {
        result = result * 2;
    }

    if (0) {
        result = result * 3;
    }

    if (1) {
        result = result * 5;
    } else {
        result = result * 7;
    }

    if (0) {
        result = result * 11;
    } else {
        result = result * 13;
    }
    return result;
}
