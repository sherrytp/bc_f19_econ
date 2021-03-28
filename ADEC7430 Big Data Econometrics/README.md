# Python Installation 
## MacOS
1. If you haven't already, add "Terminal" to your launch bar; we'll use it a lot
2. Download the Anaconda macOS Installer:
* Navigate to https://www.anaconda.com/download/#maco 
* Look for the Python 3.7 version, 64-bit Graphical Installer
3. Install Anaconda only for you (there will be one option)
* "This package will run a program to determine if the software can be installed": just press Continue
* Click "Continue" through the "Introduction", "Read Me" and "License" sections (Agree to the terms in "License")
* In "Destination Select" or "Installation Type", when you see a choice for "Change Install Location", press it
* In the "Select a Destination", select "Install on a specific disk", then choose the main disk ("Macintosh HD")
* Select "Choose Folder"
* Click "Users", then (in the next column) select your user name
* If you have a folder called "Software" in the next column, select that one. Otherwise, click "New Folder" and create the folder "Software". 
* Click "Select" and move on to the Installation
* Skip (do not choose) "Install Microsoft VSCode"
* Keep the installer, you may need it later...
* Note the final location of the Anaconda install, you'll need it later: it should be /Users/<youruser>/Software/anaconda3/
4. Create some shortcuts that will help later on:
* Following these instructions (top voted answer)
* Open a "Terminal"
* Execute the following commands:
```shell
touch /Applications/spyder
chmod +x /Applications/spyder
echo -e '#!/bin/bash'"\n/Users/<youruser>/Software/anaconda3/bin/spyder &\nexit" >> /Applications/spyder
ln -s /Applications/spyder /usr/local/bin/spyder
```
* Finally, in the terminal type "spyder", press Enter, and Spyder should launch

## Windows
1. Download the Anaconda Windows Installer:
* Navigate to https://www.anaconda.com/download/#windows 
* Look for the Python 3.7 version, 64-bit Graphical Installer
2. Install Anaconda:
* Double-click on the downloaded .exe file to start the process
* At "License Agreement", select "I Agree"
* At "Select Installation Type", select "Just Me (recommended)"
* At "Choose Install Location", click in "Browse..."
* In the "Browse for Folder" file selector, make sure the selected folder is your "username" (usually immediately under "Desktop", or with a "OneDrive" folder preceding it)
* Click on "Make New Folder" and type in the folder name "Software"
* Click again "Make New Folder", and type in "anaconda3" for the folder name
* Click "OK", then "Next"
* No changes to the "Advanced Installation Options" (i.e. unselected "Add Anaconda to my PATH...", and selected "Register Anaconda as my default Python 3.7")
* Click "Install"
* After the installation is complete, click "Next"; select "Skip" when offered to "Install Microsoft VSCode"
* Click "Finish"
* Make a note of the installation path: C:/Users/<youruser>/Software/anaconda3/
3. You may launch "Spyder" from the "Start" Windows menu (press Windows key), expand Anaconda3 (64-bit), click on "Spyder"
* You may want to create a shortcut to the Desktop, or pin Spyder to the launchbar

## Additional Setup 
**Note**: below, by "XYZ", I mean your user name - the account you login into your laptop or desktop. In Windows, your directory looks like "C:\Users\XYZ", while in macOS your main directory will be "/Users/XYZ".

1. We will follow the official Anaconda instructions to create one environment for the first few lessons. We will create a few more environments as the semester goes on, so keep these instructions handy.
2. Open a command prompt:
* In Windows: Open "Anaconda prompt". It should start with a prompt reading "(base) C:\Users\XYZ".
* In macOS: open a Terminal window. It will "know" about (ana)conda.
3. Type "conda create --name env01 python=3.6" and answer "y" after the few seconds of waiting. This will download and create a whole functioning "copy" of Python and related packages. This particular version will be Python 3.6. We may install other ones later. Note already that Anaconda came with 3.7. So you have flexibility in working with several versions of Python.
4. After the installation completes, check your environments: "conda info --envs". You should see the "base" and the "env01" environments already.
5. Activate the newly created environment:
* In Windows type "activate env01". You should notice the prompt changing to "(env01) C:\Users\XYZ" to indicate you switched to the new environment.
* In Linux(or MacOS) type "conda activate env01". Similar change to the prompt to signal switch to "env01".
6. Install a few packages. In a corresponding terminal, type: "conda install --name env01 spyder". Note that you don't have to be in the "env01" environment - conda will take care of the appropriate installation for you. Press "y" to launch the installation.
7. After the installation completes, test the launch of spyder. Type first "activate env01", hit enter, wait, then type "spyder". You should see the IDE open.
8. Repeat step 5 to install "scipy", "pandas", "numpy", "matplotlib".

More mini-lessons to follow. 
