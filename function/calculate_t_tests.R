# 定义计算两两组之间t检验的函数
calculate_t_tests <- function(data, group_by, value_column) {
  # data: 数据框
  # group_by: 组别的列名
  # value_column: 要比较的数值的列名
  
  # 提取唯一的组别
  groups <- unique(data[[group_by]])
  
  # 初始化结果矩阵
  result_matrix <- matrix(NA, nrow = length(groups), ncol = length(groups),
                          dimnames = list(groups, groups))
  
  # 循环遍历所有可能的组合
  for (i in 1:(length(groups)-1)) {
    for (j in (i+1):length(groups)) {
      # 提取两组数据
      group1_data <- data[data[[group_by]] == groups[i], value_column]
      group2_data <- data[data[[group_by]] == groups[j], value_column]
      
      # 执行t检验
      t_test_result <- t.test(group1_data, group2_data)
      
      # 提取p值
      p_value <- t_test_result$p.value
      
      # 将p值存入结果矩阵
      #result_matrix[groups[i], groups[j]] <- p_value
      result_matrix[groups[j], groups[i]] <- p_value
    }
  }
  
  # 返回结果矩阵
  return(result_matrix)
}