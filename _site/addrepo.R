if (!requireNamespace("httr")) install.packages("httr")
if (!requireNamespace("svDialogs")) install.packages("svDialogs")

library(httr)
library(svDialogs)

# GitHub token and organization name
email <- "email"
name <- "name"
token <- "token"
org_name <- "lmu-gp2"

# Dialog inputs for repository details
repo_name <- dlg_input("Repository name", "...-analysis")$res
description <- dlg_input("Repository description", "Description of the new repository")$res
#collaborators <- dlg_list(c("mrzdcmps", "test"), title = "Add collaborators", multiple = TRUE)$res

# Function to create a new GitHub repository
create_repo <- function(token, org_name, repo_name, description) {
  url <- paste0("https://api.github.com/orgs/", org_name, "/repos")
  headers <- add_headers(Authorization = paste("token", token), Accept = "application/vnd.github.v3+json")
  body <- list(name = repo_name, description = description, private = TRUE)
  response <- POST(url, headers, body = body, encode = "json")
  
  if (status_code(response) == 201) {
    message("Repository created successfully!")
    return(content(response)$clone_url)
  } else if (status_code(response) == 422 && grepl("already exists", content(response)$errors[[1]]$message)) {
    message("Repository already exists.")
    return(paste0("https://github.com/", org_name, "/", repo_name, ".git"))
  } else {
    stop("Failed to create repository: ", content(response)$message)
  }
}

# Function to add collaborators to the repository
add_collaborators <- function(token, org_name, repo_name, collaborators) {
  headers <- add_headers(Authorization = paste("token", token), Accept = "application/vnd.github.v3+json")
  
  for (collaborator in collaborators) {
    url <- paste0("https://api.github.com/repos/", org_name, "/", repo_name, "/collaborators/", collaborator)
    response <- PUT(url, headers)
    
    if (status_code(response) == 201 || status_code(response) == 204) {
      message(paste("Added", collaborator, "as a collaborator."))
    } else {
      message(paste("Failed to add", collaborator, ":", content(response)$message))
    }
  }
}

# Function to initialize Git in the local RStudio project (if not already initialized)
initialize_git <- function() {
  if (!file.exists(".git")) {
    system("git init")
    message("Initialized empty Git repository.")
  } else {
    message("Git repository already initialized.")
  }
}

# Function to add the remote repository to the local Git configuration
add_remote <- function(remote_url) {
  remotes <- system("git remote", intern = TRUE)
  if (!"origin" %in% remotes) {
    system(paste("git remote add origin", remote_url))
    message("Added remote repository: ", remote_url)
  } else {
    message("Remote repository already added.")
  }
}

# Function to create a .gitignore file with specified content
create_gitignore <- function() {
  gitignore_content <- "
.Rproj.user
.Rhistory
.RData
.Ruserdata
*.Rproj
config.yml
.gitignore
"
  if (!file.exists(".gitignore")) {
    writeLines(gitignore_content, ".gitignore")
    message(".gitignore file created.")
  } else {
    message(".gitignore file already exists.")
  }
}

# Function to stage all files, make an initial commit, and push to the remote repository
initial_commit_and_push <- function() {
  status <- system("git status --porcelain", intern = TRUE)
  if (length(status) > 0) {
    system(paste0("git config user.email \"",email,"\""))
    system(paste0("git config user.name \"",name,"\""))
    system("git add .")
    system("git commit -m \"Initial commit\"")
    system("git branch -M main")  # Ensure the branch is named 'main'
    system("git push -u origin main")
    message("Initial commit pushed to remote repository.")
  } else {
    message("No changes to commit. Skipping commit and push.")
  }
}

# Execute the steps with error handling
tryCatch({
  remote_url <- create_repo(token, org_name, repo_name, description)
  #add_collaborators(token, org_name, repo_name, collaborators)
  initialize_git()
  add_remote(remote_url)
  create_gitignore()
  initial_commit_and_push()
  message("All steps completed successfully!")
}, error = function(e) {
  message("An error occurred: ", e$message)
})