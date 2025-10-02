# Lazyman Scripts
這邊是我放資料處理上原本會很煩的步驟，讓他儘量能one-liner解決。  
2025/09/25開始施工.....

## Easy data processing
1. **convert_wide_to_long.R and convert_long_to_wide.R**
這2個function可以把你的寬表直接變成長表 or _vice verse_。  
只要指定**id_cols**，就可以把除了指定的ID columns外都拉下去。  
可以選擇要不要把沒有資訊的部份刪除，適合用在sparsity很高的資料。
出來的結果value column會分成numeric, characteristics, logic，以容納各種類型的type，並且有一個data_type column提醒讓你不要抓錯。  
```R
tmp <- convert_wide_to_long(data, id_cols = c("YOUR ID COLUMNS in a VECTOR"), remove_empty = TRUE)
as.data.frame(tmp)
```
相反的，可以把轉出的long table轉回wide table
```R
convert_long_to_wide(long_result, id_cols = c("s_id", "tp_id"))
```

2. **check_data_identity.R**
這個function可以讓你check兩份data是否有不同。
Column順序不一樣沒關係，他會去找一樣的column name去比對。
但若順序的確不同時，他還是會warning說順序不同。
```R
check_data_identity(data1, data2)
```



