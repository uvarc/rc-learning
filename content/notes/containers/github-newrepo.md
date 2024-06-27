---
title: Creating a New GitHub Repository
date: "2023-05-01:00:00Z"
draft: false  # Is this a draft? true/false
toc: false  # Show table of contents? true/false
type: docs  # Do not modify.
weight: 310
date: "2023-05-01T00:00:00Z"
menu:
  containers:
      parent: Version Control
---

There are several ways to create a new GitHub repository. Here is one way I like to do it:

1. Create a new repository with the "New repository" button.

2. Fill out the "Repository name".

3. Select "Add a README file".

4. Click "Create repository".

5. In your local terminal, clone the new repository with the following command: 

    ```
    git clone https://github.com/<username>/<repo-name>
    ```
    
6. Copy or move the files you want to the <repo-name> folder.

7. Run the following code to add the new files to your GitHub repository:

    ```
    cd /path/to/<repo-name>
    
    git add *
    
    git commit -m "adding files"
    
    git push
    ```