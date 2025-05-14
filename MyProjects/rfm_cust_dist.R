# Customer segmentation - RFM Analysis - Recency, Frequency, Monetary Analysis

# Recency - How recently a customer purchased or transacted
# Frequency - How frequently customers purchaisng or transacting
# Monetory - How much money a customer spending or transacting
# 80% of business comes from 20% of Customers

library(rfm)
library(ggplot2)
rfm_data_customer # Default dataset (Cutomer_id, Revenue, MostRecentVisit, NumberofOrders, recencyDays)
# RFM score
# Recency Score - 5 to 1 - 5 most recent to 1 least recent
# Frequency Score - 5 to 1 - 5 most frequent to 1 least frequent
# Monetory Score - 5 to 1 - 5 most spending to 1 least spending
# RFM Score - Concatenation of all 3 score above (R, F, M)

library(lubridate)
library(tidyverse)

data("rfm_data_customer")
View(rfm_data_customer)

analysis_date = as_date('2006-12-31')

#rfm_result = rfm_table_order(rfm_data_orders, customer_id, order_date, revenue, analysis_date)

# Create RFM Table:
rfm_table <- rfm_data_customer %>%
  group_by(customer_id) %>%
  summarise(
    recency = as.numeric(analysis_date - (most_recent_visit)),
    frequency = n(),
    monetary = sum(revenue)
  )
head(rfm_table)


# Create RFM Scores (1-5)
names(rfm_data_customer)




rfm_result <- rfm_table_order(
  data = rfm_data_customer,
  customer_id = customer_id,
  order_date = most_recent_visit,
  revenue = revenue,
  analysis_date = analysis_date
)

# Visulaize the Segments:
rfm_heatmap(rfm_result)
rfm_plot_bar_chart(rfm_result)
#rfm_histograms(rfm_result)
rfm_plot_histogram(rfm_result, metric = "recency_days")
rfm_plot_histogram(rfm_result, metric = "frequency")
rfm_plot_histogram(rfm_result, metric = "monetary")



rfm_result
rfm_heatmap(rfm_result)
rfm_bar_chart(rfm_result)
rfm_histograms(rfm_result)
rfm_order_dist(rfm_result)

# Scatter Plots - Relationship between 2 variables
# Recency vs Monetary
rfm_plot_scatter(rfm_result)







