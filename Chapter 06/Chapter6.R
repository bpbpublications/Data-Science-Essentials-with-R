
# load tidyverse
#library(tidyverse)

library(dplyr)

# install stringr
install.packages("stringr")

# load stringr
library(stringr)

# Create a data frame of product data
books <- data.frame(product_name = c("Mastering MEAN Stack", " Breaking Ransomware", "Data Science with R ", "Learning Google Cloud Vertex AI"),
                    product_category = c("Web Development ", " Security ", "Machine Learning & AI", "Cloud & Networking"),
                    product_id = c(255, 78, 7077, 537))

# Display the data frame
View(books)

#  Changing case of product_name and product_category 
books <- books %>%
  mutate(product_name = str_to_lower(product_name),
         product_category = str_to_lower(product_category))

# Display books
View(books)

# Calculate the length of the product_name
books <- books %>%
  mutate(product_name_length = str_length(product_name))

# View the modified data frame
View(books)

# Replacing substring from the column product_category
books <- books %>%
  mutate(product_category = str_replace_all(product_category, "&", ""))

# View the data frame
View(books)

# Remove leading and trailing spaces in product_name and product_category
books <- books %>%
  mutate(product_name = str_trim(product_name),
         product_category = str_trim(product_category))

# Display the modified data frame
View(books)

# Apply str_squish() to product_category column
books <- books %>%
  mutate(product_category = str_squish(product_category))

# View the data frame
View(books)


# Replacing all spaces in product names and categories with hyphens
books <- books %>%
  mutate(product_name = str_replace_all(product_name, " ", "-"),
         product_category = str_replace_all(product_category, " ", "-"))

# Display the dataframe
View(books)


# Using str_pad() to standardize product_id with leading zeros
books <- books %>%
  mutate(product_id = str_pad(product_id, width = 4, side = "left", pad = "0"))

# View the dataframe
View(books)

# Using str_c() to generate unique product URLs
books <- books %>%
  mutate(product_url = str_c("https://in.bpbonline.com/", "collections/", product_category, "/", product_name, "/", product_id))


# Display the product_url column
books %>% select(product_url)


# Extracting the last 4 digits from the product_url column.
books <- books %>%
  mutate(extracted_product_id = str_sub(product_url, start = -4))


# Print the extracted_product_id
books %>% select(extracted_product_id)


# Sample HTML text 
text <- "<div class='main'><h1>Machine Learning & AI</h1><div> <a href='DataSciencewithR.pdf'>Data Science with R</a> </div></div>"

# print the ‘text’
text


# Greedy matching
str_extract(text, "<div>.*</div>")


# Non Greedy matching
str_extract(text, "<div>.*?</div>")


# Create a dataframe, “bank_messages”
bank_messages <- data.frame(
  message = c(
    "Your Bank Acc XX998006 is debited for INR 555.68 on 10-Nov-23 towards paytmqroihh5855ap@paytm. UPI Ref No If you have any concerns, please contact us at 1800-267-3456.",
    "Dear Customer, Your A/C XXXXX123456 has a credit by Cheque of Rs 80,000.00 on 30/10/23."
  )
)


# Extracts the amount preceded by “INR” or "Rs”
bank_messages <- bank_messages %>%
  mutate(amount = str_extract(message, "(?<=\\bINR\\s|\\bRs\\s)[0-9,]+\\.?[0-9]*"))

# Check the result
View(bank_messages)


# Add columns to indicate whether the message contains “credit” or “debit”
bank_messages <- bank_messages %>%
  mutate(
    has_credit = str_detect(message, "\\bcredit(ed)?\\b"),
    has_debit = str_detect(message, "\\bdebit(ed)?\\b")
  )


# Extract date from the message column
bank_messages <- bank_messages %>%
  mutate(
    date = str_extract_all(message, "\\b\\d{2}/\\d{2}/\\d{2}|\\d{2}-[a-zA-Z]{3}-\\d{2}\\b")
  )


# Extract phone number
bank_messages <- bank_messages %>%
  mutate(
    phone_number = str_extract(message, "\\b\\d{4}-\\d{3}-\\d{4}\\b")
  )

# View the dataframe
View(bank_messages)


