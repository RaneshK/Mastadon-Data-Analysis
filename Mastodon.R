# Load the required 'rtoot' libary so that we can use the Mastodon API
library(rtoot)
# Authenticate our RStudio session to use the Mastodon API using our Mastodon Account
auth_setup()

m#1
# Get toots from Mastodon and store the data in the variable swa.scc.posts
swa.scc.posts = get_timeline_hashtag(hashtag = "apple", limit = 1000, instance = "mastodon.social")

#2
#a
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
contingency_table_df


# e
# Since our contingency table contains empty columns we can remove those to be able to 
# perform a chi-squared test
# Remove empty entries from the contingency table to perform a chi-squared test
no_zeros_contingency_table_df <- data.frame(
  Sun = c(13, 90, 3),
  Mon = c(51, 209, 28),
  Tue = c(40, 253, 33),
  Wed = c(34, 228, 18),
  row.names = c("High", "Low", "Med")
)

# View our new contingency table without any empty entries
no_zeros_contingency_table_df

result <- chisq.test(no_zeros_contingency_table_df)

cat("The chi-squared test statistic (x2) is: ", result$statistic, "\n")
cat("The p-value is: ", result$p.value, "\n")
cat("The degrees of freedom is: ", result$parameter, "\n")







########################################################################################################################

# Text Mining

# Load the necessary libraries
library(tm)
library(slam)

# Get the content from our downloaded mastodon toots and store in in a variable called posts.text
posts.text = swa.scc.posts$content

posts.text.corpus = Corpus(VectorSource(posts.text))

posts.text.corpus = tm_map(posts.text.corpus, function(x) iconv(x, to='UTF-8-MAC', sub = 'byte'))

corpus = tm_map(posts.text.corpus, function(x) iconv(x, to='ASCII', sub = ' '))
corpus = tm_map(corpus, removeNumbers)
corpus = tm_map(corpus, removePunctuation)
corpus = tm_map(corpus, stripWhitespace)
corpus = tm_map(corpus, tolower)
corpus = tm_map(corpus, removeWords, stopwords())
corpus = tm_map(corpus, stemDocument)

clean_html <- function(text) {
  # Remove anything between angle brackets
  text <- gsub("<.*?>", "", text)
  # Remove URLs
  text <- gsub("http\\S+|www\\S+|https\\S+", "", text)
  # Remove mentions (e.g., @username)
  text <- gsub("@\\S+", "", text)
  # Remove specific HTML attributes
  text <- gsub("\\bclass\\s*=\\s*\"mention\"|\\bref\\s*=\\s*\"tag<>span\"|\\btarget\\s*=\\s*\"_blank\"", "", text, ignore.case = TRUE)
  # Remove hashtags
  text <- gsub("#\\S+", "", text)
  # Remove underscores
  text <- gsub("_", "", text)
  # Remove specific words
  text <- gsub("\\b(?:mastodon|https)\\b", "", text, ignore.case = TRUE)
  return(text)
}


# Function to remove non-English words
removeNonEnglishWords <- function(text) {
  words <- unlist(strsplit(text, "\\s+"))
  words <- words[words == tools::toTitleCase(words, "en")]
  text <- paste(words, collapse = " ")
  return(text)
}

# Define a custom function to remove HTML tags and URLs
clean_html <- function(text) {
  # Remove HTML tags
  text <- gsub("<.*?>", "", text)
  # Remove URLs
  text <- gsub("http\\S+|www\\S+|https\\S+", "", text)
  return(text)
}

# Apply the custom cleaning function to the Corpus
posts.text.corpus <- tm_map(posts.text.corpus, content_transformer(clean_html))


posts.text.corpus

# Create a Document Term Matrix with TFIDF weights
# Create a document-term matrix
dtm <- DocumentTermMatrix(corpus)

# Calculate TFIDF weights
tfidf <- weightTfIdf(dtm)

# Ensure empty documents are not included in the final matrix by removing any empty documents
non_empty_docs <- row_sums(tfidf) > 0
non_empty_tfidf <- tfidf[non_empty_docs, ]

# View the resulting TFIDF matrix
print(non_empty_tfidf)


# Construct a binary distance matrix
binary_distance_matrix <- as.matrix(dist(non_empty_tfidf, method = "binary"))

# View the binary distance matrix
print(binary_distance_matrix)



# 5
pca <- prcomp(binary_distance_matrix)

# Reduce the dimensions of the data set (50%)
pca_scores <- as.matrix(pca$x[,1:2])

# Extract the proportion of variance explained by each principal component
explained_variance <- pca$sdev^2 / sum(pca$sdev^2)

# Calculate the cumulative proportion of variance explained
cumulative_explained_variance <- cumsum(explained_variance)

# Plot the cumulative explained variance
plot(cumulative_explained_variance, type = "b", pch = 19, frame = FALSE,
     xlab = "Number of Principal Components", ylab = "Cumulative Explained Variance",
     main = "Cumulative Explained Variance for Optimal Number of Clusters")

# Add a horizontal line at 95% explained variance (adjust threshold as needed)
abline(h = 0.95, col = "red", lty = 2)

# Find the number of principal posts that exceed the threshold
optimal_num_posts <- which(cumulative_explained_variance >= 0.95)[1]

# Add a vertical line and text annotation for the optimal number of posts
abline(v = optimal_num_posts, col = "blue", lty = 2)
text(optimal_num_posts, 0.95, labels = paste("Optimal number of posts =", optimal_num_posts), pos = 4, col = "blue")

#6
# Cluster the documents using k-means clustering
# Initialize Within-cluster sum of squares vector with 10 clusters
wcss <- numeric(length = 10)

# Iterate over different values of k (number of clusters)
for (k in 1:10) {
  # Perform k-means clustering
  kmeans_result <- kmeans(pca_scores, centers = k, nstart = 10)
  
  # Store within-cluster sum of squares in the WCSS vector
  wcss[k] <- kmeans_result$tot.withinss
}

# Plot the elbow curve
plot(1:10, wcss, type = "b", pch = 19, frame = FALSE, 
     xlab = "Number of Clusters (k)", ylab = "WCSS",
     main = "Elbow Method for Optimal Number of Posts")

# Add text annotation for the elbow point
elbow_point <- which(diff(wcss) < 0.05 * max(diff(wcss)))[1]
points(elbow_point, wcss[elbow_point], col = "red", cex = 2, pch = 20)
text(elbow_point, wcss[elbow_point], labels = paste("Elbow Point (k =", elbow_point, ")"), pos = 2, col = "red")


# 7
# Perform k-means clustering with the optimal number of clusters
optimal_kmeans_result <- kmeans(pca_scores, centers = optimal_num_posts, nstart = 10)

# Create a data frame with PCA scores and cluster assignment
cluster_data <- data.frame(x = pca_scores[, 1], y = pca_scores[, 2], cluster = as.factor(optimal_kmeans_result$cluster))

# Plot the clusters
library(ggplot2)
ggplot(cluster_data, aes(x = x, y = y, color = cluster)) +
  geom_point(size = 3) +
  labs(title = "2-Dimensional Clustering Visualization", x = "Principal Component 1", y = "Principal Component 2") +
  theme_minimal()


# 8
# Create a word cloud of the largest cluster
# Identify the largest cluster
largest_cluster <- which.max(table(optimal_kmeans_result$cluster))

# Extract content for the largest cluster
largest_cluster_indices <- which(optimal_kmeans_result$cluster == largest_cluster)
largest_cluster_content <- posts.text[largest_cluster_indices]

# Combine the text into a single character vector
text_for_wordcloud <- paste(largest_cluster_content, collapse = " ")

# Create a word cloud
library(wordcloud)
wordcloud(words = names(table(unlist(strsplit(text_for_wordcloud, "\\s+")))),
          freq = table(unlist(strsplit(text_for_wordcloud, "\\s+"))),
          min.freq = 2, # Minimum frequency for a word to be included
          scale = c(3, 0.5), # Scaling of font sizes
          colors = brewer.pal(8, "Dark2"), # Color palette
          random.order = FALSE, # Display words in decreasing frequency
          max.words = 100, # Maximum number of words to display
          random.color = TRUE, # Use random colors
          rot.per = 0.35, # Probability of word rotation
          main = paste("Word Cloud for Largest Cluster (Cluster ", largest_cluster, ")"))




#9
# Preprocess the text in the largest cluster
preprocess_text <- function(text) {
  # Remove anything between angle brackets
  text <- gsub("<.*?>", "", text)
  # Remove URLs
  text <- gsub("http\\S+|www\\S+|https\\S+", "", text)
  # Remove mentions (e.g., @username)
  text <- gsub("@\\S+", "", text)
  # Remove specific HTML attributes
  text <- gsub("\\bclass\\s*=\\s*\"mention\"|\\bref\\s*=\\s*\"tag<>span\"|\\btarget\\s*=\\s*\"_blank\"", "", text, ignore.case = TRUE)
  # Remove hashtags
  text <- gsub("#\\S+", "", text)
  # Remove underscores
  text <- gsub("_", "", text)
  # Remove specific words
  text <- gsub("\\b(?:mastodon|https)\\b", "", text, ignore.case = TRUE)
  return(text)
}

preprocessed_cluster_text <- sapply(largest_cluster_content, preprocess_text)

# Create a document-term matrix with binary weights
dtm_binary <- DocumentTermMatrix(Corpus(VectorSource(preprocessed_cluster_text)), control = list(weighting = weightBin))

# Convert to a matrix
mat_binary <- as.matrix(dtm_binary)

# Calculate binary distance
binary_dist <- dist(t(mat_binary), method = "binary")

# Hierarchical clustering with single linkage
cluster_tree <- hclust(binary_dist, method = "single")

# Create a dendrogram plot
plot(cluster_tree, main = "Dendrogram of Frequently Used Words in Largest Cluster", xlab = "Words", sub = "Single Linkage Clustering")




# Social Network
#10
# Find the row numbers where in_reply_to is not NULL
quote.posts <- which(!is.na(swa.scc.posts$in_reply_to_id))

# View the resulting row numbers
quote.posts

# Find the row numbers where in_reply_to is not NULL
quote.posts <- which(!is.na(swa.scc.posts$in_reply_to_id))

# Extract the usernames of users who are quoted using lapply and sapply
who.quoted <- sapply(lapply(swa.scc.posts$account[quote.posts], function(acc) acc$username), as.character)

# View the resulting usernames
who.quoted

#12

# Create an empty list to store original quoted posts
original.quoted.posts.list <- list()

# Verify that original.quoted.posts.list is a list
if (is.list(original.quoted.posts.list)) {
  print("original.quoted.posts.list is a list.")
} else {
  print("original.quoted.posts.list is not a list.")
}

# Loop over each quoted post
for (quote_index in quote.posts) {
  # Get the information of the original post that was quoted
  original_post <- swa.scc.posts[swa.scc.posts$id == swa.scc.posts$in_reply_to_id[quote_index], ]
  
  # Add the original post information to the list
  original.quoted.posts.list[[paste0("Quote_", quote_index)]] <- original_post
}

# View the list of original quoted posts
original.quoted.posts.list

# Extract the first object from the list
first_object <- original.quoted.posts.list[[1]]

# Get the names of attributes
attribute_names <- names(first_object)

# Print the attribute names
print(attribute_names)


# 13
# Initialize the variable to store original authors
original.author <- character(length = length(original.quoted.posts.list))

# Loop over each quoted post
for (i in seq_along(original.quoted.posts.list)) {
  # Get the content of the quoted post
  quoted_content <- original.quoted.posts.list[[i]]
  
  # Extract the username from the content (assuming a specific pattern)
  # Adjust the pattern based on your data structure
  original_author <- regmatches(quoted_content, regexpr("@\\w+", quoted_content))
  
  # Store the original author in the variable
  original.author[i] <- ifelse(length(original_author) > 0, original_author, NA)
}

# Remove NAs from the variable
original.author <- na.omit(original.author)

# View the variable with original authors
print(original.author)

# 14

# Initialize the edge list
edge_list <- data.frame(
  quoted_post_content = character(),
  original_author = character(),
  stringsAsFactors = FALSE
)

# Use Map to iterate over elements in original.quoted.posts.list and original.author
edge_list <- do.call(rbind, Map(function(posts, author) {
  data.frame(
    quoted_post_content = unlist(posts),
    original_author = rep(author, length(unlist(posts))),
    stringsAsFactors = FALSE
  )
}, original.quoted.posts.list, original.author))

# Remove rows with empty entries in any column
edge_list <- edge_list[rowSums(edge_list == "") == 0, ]

# View the edge list
print(edge_list)



#15

# Create a graph from our edge list
quote.network = graph.data.frame(edge_list)
# Plot the graph
plot(quote.network)

# Since our original graph is very dense we are going to need to plot a subgraph
if (ecount(quote.network) > 100) {
  top.vertices <- order(degree(quote.network), decreasing = TRUE)[1:10]
}
# Create a sub graph with the top 10 vertices
quote.network.sub = induced_subgraph(quote.network, top.vertices)
# Plot the subgraph
plot(quote.network.sub)


# 16

# Assuming 'quote.network.sub' is the social network graph
library(igraph)

# Calculate closeness centrality
closeness_centrality <- closeness(quote.network.sub, mode = "all")

# Order users by closeness centrality in descending order
top_users <- order(closeness_centrality, decreasing = TRUE)[1:10]

# Get usernames corresponding to the top users
top_usernames <- V(quote.network.sub)$name[top_users]

# Print the top 10 central users and their closeness centrality
for (i in seq_along(top_usernames)) {
  cat("User:", top_usernames[i], "\tCloseness Centrality:", closeness_centrality[top_users[i]], "\n")
}

