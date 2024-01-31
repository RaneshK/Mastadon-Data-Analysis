library(cld3)
install.packages("proxy")


# Load the required 'rtoot' libary so that we can use the Mastodon API
library(rtoot)
# Authenticate our RStudio session to use the Mastodon API using our Mastodon Account
auth_setup()

#1
# Get toots from Mastodon and store the data in the variable swa.scc.posts
swa.scc.posts = get_timeline_hashtag(hashtag = "apple", limit = 1000, instance = "mastodon.social")

# Define the language identification function
#identify_language <- function(text) {
 # result <- cld3::detect_language(text)
  # Check if result is a list or a data frame with a 'language' component
#  if (is.list(result) || is.data.frame(result)) {
#    if ("language" %in% names(result)) {
#      return(result$language == "en")
#    }
#  }
  # Assuming non-list, non-data.frame results should be treated as non-English
#  return(FALSE)
#}

# Filter the data to keep only English content
#swa.scc.posts <- swa.scc.posts[sapply(swa.scc.posts$content, identify_language

#2
# Get the unique users from the downloaded toots using the lapply library
# Get all the users from our downloaded toots using the lapply library
users <- lapply(swa.scc.posts$account, function(acc) {
  acc$username
})

# Create a list of unique usernames by removing the duplicate users
unique.users <- lapply(unique(users), as.character)

# View the unique users
unique.users

# Create an empty list to store the follower count of each unique user
followers.count <- list()

# Loop over each unique user
for (username in unique.users) {
  # Find the index of each username from the unique users list in swa.scc.posts$account to
  # get the follower count of each unique user
  index <- which(sapply(swa.scc.posts$account, function(acc) acc$username) == username)[1]
  
  # Extract the follower count of each unique user using the index
  follower_count <- swa.scc.posts$account[[index]]$followers_count
  
  # Add the follower count of each unique user to the variable followers.count
  followers.count[[username]] <- follower_count
}

# View the follower count of each unique user
followers.count


#b
# Convert the 'created_at' timestamps to a POSIXlt object 
swa.scc.posts$created_at <- as.POSIXlt(swa.scc.posts$created_at, format="%Y-%m-%dT%H:%M:%S", tz = "UTC")

# Create an empty list to store the number of posts for each unique user and each day of the week
user.postdays <- list()

# Define the days of the week
days_of_week <- c("Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat")

# Loop over each unique user
for (username in unique.users) {
  # Find the indices of each username from the unique users in swa.scc.posts$account
  indices <- which(sapply(swa.scc.posts$account, function(acc) acc$username) == username)
  
  # Create a data frame to store the count of posts for each day of the week
  user.posts <- data.frame(day = days_of_week, posts = rep(0, length(days_of_week)))
  
  # Loop over the indices and update the data frame
  for (index in indices) {
    # Extract the day of the week (0 = Sun, 1 = Mon, ..., 6 = Sat)
    day_of_week <- weekdays(swa.scc.posts$created_at[index], abbreviate = TRUE)
    
    # Increment the count for the corresponding day of the week
    user.posts$posts[user.posts$day == day_of_week] <- user.posts$posts[user.posts$day == day_of_week] + 1
  }
  
  # Add the posts for each day of the week for each unique user
  user.postdays[[username]] <- user.posts
}

# View the number of posts for each unique user and each day of the week
user.postdays

# Create a matrix to store the number of posts for each user and each day of the week
users.postdays <- matrix(0, nrow = length(unique.users), ncol = length(days_of_week), dimnames = list(unique.users, days_of_week))

# Loop over each unique user
for (i in seq_along(unique.users)) {
  username <- unique.users[[i]]
  
  # Find the indices of each username from the unique users list in swa.scc.posts$account
  indices <- which(sapply(swa.scc.posts$account, function(acc) acc$username) == username)
  
  # Loop over the indices and update the matrix
  for (index in indices) {
    # Extract the day of the week (0 = Sun, 1 = Mon, ..., 6 = Sat)
    day_of_week <- weekdays(swa.scc.posts$created_at[index], abbreviate = TRUE)
    
    # Increment the count in the matrix
    users.postdays[username, day_of_week] <- users.postdays[username, day_of_week] + 1
  }
}

# View the matrix users.postdays
users.postdays

#c
# Categorise the users based on their follower counts 
followers.category = ifelse(followers.count <=500, "Low",
                            ifelse(followers.count <1000, "Med", "High"))

# View the categorization of the unique users based on their follower counts
followers.category

# d
# Define the days of the week

# Create an empty contingency table matrix
contingency_table <- matrix(0, nrow = 3, ncol = 7, dimnames = list(c("High", "Low", "Med"), days_of_week))

# Loop over each unique user
for (i in seq_along(unique.users)) {
  username <- unique.users[[i]]
  follower_category <- followers.category[[i]]
  
  # Loop over each day of the week
  for (day_index in seq_along(days_of_week)) {
    day <- days_of_week[day_index]
    
    # Get the count of posts for the user on the specific day
    posts_count <- users.postdays[username, day]
    
    # Update the count in the contingency table
    contingency_table[follower_category, day_index] <- contingency_table[follower_category, day_index] + posts_count
  }
}

# Convert the matrix to a data frame for better readability
contingency_table_df <- as.data.frame(contingency_table)

# View the contingency table
print(contingency_table_df)

result <- chisq.test(contingency_table)

# Perform chi-squared test of independence
cat("Chi-squared test statistic: ", result$statistic, "\n")
cat("p-value: ", result$p.value, "\n")
cat("Degrees of freedom: ", result$parameter, "\n")

print(result)

# Load necessary libraries
library(tm) # for text mining
library(SnowballC) # for text stemming
library(slam) # for sparse matrix operations

# Extract Text Data
posts.text <- swa.scc.posts$content

#Create Corpus
corpus <- Corpus(VectorSource(posts.text))

# Pre-processing steps: convert to lowercase, remove punctuation, numbers, stopwords, and whitespace
corpus <- tm_map(corpus, content_transformer(tolower))
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, removeNumbers)
corpus <- tm_map(corpus, removeWords, stopwords("english"))
corpus <- tm_map(corpus, stripWhitespace)

#3 Construct Document Term Matrix with TFIDF weights
# Create a Term-Document Matrix
tdm <- TermDocumentMatrix(corpus)
tdm_matrix <- as.matrix(tdm)


# Calculate Term Frequency (TF)
tf <- tdm_matrix / rowSums(tdm_matrix)

# Calculate Inverse Document Frequency (IDF)
idf <- log2(nDocs(tdm) / rowSums(tdm_matrix > 0))

# Calculate TFIDF
tfidf_matrix <- tf * idf

dim(tfidf_matrix)

# Adjust sparsity threshold as needed
tdm <- removeSparseTerms(tdm, sparse = 0.99)  

View(tfidf_matrix)

# Convert TFIDF matrix to binary
binary_matrix <- as.matrix(tfidf_matrix > 0)

# Compute the binary distance matrix
# Here, we use 'dist' function with method 'manhattan' which is suitable for binary data
binary_distance_matrix <- dist(binary_matrix, method = "manhattan")

mds_result <- cmdscale(binary_distance_matrix, k = 2)  # k is the number of dimensions

max_clusters <- 10  # Define the maximum number of clusters to consider
wss <- numeric(max_clusters)

for (i in 1:max_clusters) {
  set.seed(123)  # For reproducibility
  cluster_result <- kmeans(mds_result, centers = i, nstart = 25)
  wss[i] <- cluster_result$tot.withinss
}

plot(1:max_clusters, wss, type = "b", xlab = "Number of Clusters", ylab = "Within-Cluster Sum of Squares",
     main = "Elbow Method for Determining Optimal Number of Clusters")

