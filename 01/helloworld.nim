echo "hello world!"

block outer:
    for i in 0..5:
        block inner:
            for j in 0..5:
                echo i, j
                break inner
