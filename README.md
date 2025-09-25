# My Scripts
This is my space for easy coding.......
In the FUTURE, a lot of useful functions will be provided here.

Currently, we have....

## Easy data processing
1. **convert_wide_to_long.R**
這個可以把你的寬表直接變成長表，
只要指定**id_cols**，就可以把除了指定的ID columns外都拉下去。
可以選擇要不要把沒有資訊的部份刪除，適合用在sparsity很高的資料。
```R
convert_wide_to_long(data, id_cols = c("YOUR ID COLUMNS in a VECTOR"), preserve_types = TRUE, remove_empty = TRUE)
```


