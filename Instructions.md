# GROUP_Z_ACC_PROJ_UNSW_2022


# Instructions for setting up Github and linking it to RStudio Projects
1. **Make Github Account**: Create a Github account.
2. **Install Git**: Go to https://git-scm.com/ to install git. Please make sure you know where the git folder is installed.
3. **Set Up SSH Key**: Go to RStudio: Tools -> Global Options -> Git/SVN. Tick "Enable version control...". Put your git executable file path into the space for "Git executable". Create a new SSH RSA key (If you haven't already). Click "View public key" and copy the whole key. It should look like this:
<img width="592" alt="image" src="https://user-images.githubusercontent.com/55743621/153742960-f26d2bfc-8fcd-4e5c-9269-91e8d16e4cff.png">

4. **Put your SSH Key in Github**: _(If you have set up an SSH key in Github, go to next step)_ Go to your Github account, click the top right icon, go to Settings-> "SSH and GPG keys". Click "New SSH Key", and paste the key you have just copied from RStudio and save.
5. **Get your Personal Access Token (PAT)**: Now on Github, go to your Settings -> "Developer Settings" -> "Personal access token". Click "Generate New Token", set the time period to 90 days, give it a name, and tick everything. COPY/SAVE THIS TOKEN.
6. **Putting things together on RStudio**: Going back to RStudio, Go to a directory where you would like to clone this project, click "File" -> "New Project" -> "Version Control" -> "Git"
7. **Pasting your Git clone links along with PAT**: Under Repository URL, enter the Github Clone URL in this format (Please replace the stuff in the angle brackets (including the angle brackets themselves) with the appropriate letters/characters): "https://<YOUR_GITHUB_USERNAME>:<YOUR_PAT>@github.com/alexzhu1998/GROUP_Z_ACC_PROJ_UNSW_2022.git"
8. **Subdirectory**: Browse through the computer and make sure you have the right subdirectory for this cloned repository and click "Create Project".
9. **Setting up your git identity** If you have installed git for the first time, you need to do this step. You need to tell git who you are by typing your name and email in the terminal or command prompt as follows:
```
git config --global user.email "<YOUR_GITHUB_EMAIL>"
git config --global user.name "<YOUR_GITHUB_USERNAME>"
```
10. NOW YOU ARE DONE!

# Helpful commands/buttons to use Git
1. **Pull**: The Pull function allows git to retrieve what your teammates have coded and make changes directly to your file system. If you haven't started making changes, always pull (Blue Down Arrow button under the Git window in `RStudio`) first to make sure your version of the repo is up to date.

2. **Commit**: To tell github you are planning on saving your changes into the github repository, you need to store the info into your local git. This is known as a **commit**. To do this, you first save the files, and go to the top right corner and tick all the blank boxes, and click commit. Sometimes, you will run into a merge conflict, because your version of the local repo is different to the online repo, possibly due to you not regularly pulling from Github. When that time comes, just let me know. So please click Pull regularly!
<img width="439" alt="image" src="https://user-images.githubusercontent.com/55743621/153742341-928c71c0-0075-493b-9a0b-22959386982c.png">

3. **Push**: This will open a new RStudio Version Control window, and you can see your changes. Add a commit message and push. Then everyone can see your change.
<img width="999" alt="image" src="https://user-images.githubusercontent.com/55743621/153742401-6cfeb1cb-7fd4-42df-9f9d-0142c836481c.png">

4. **Branches**: Sometimes, you might want to work on your own experimental code but you still want to push it to the repository. You can do this with branches, which can be thought of as a new chain of code split from the original code. You can make a new branch by clicking the left button encircled by the red lines, and switch between branches by clicking on the drop down on the right circle. You can also merge your changes with the main codebase, just let me know if you want to merge.
<img width="439" alt="image" src="https://user-images.githubusercontent.com/55743621/153742534-d73aca95-1764-4fe5-b982-c20f2d635874.png">
