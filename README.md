# ascii-chat
A terminal-based peer-to-peer (P2P) end-to-end-encrypted (E2EE) video chat application with messaging and audio support.

##Installation
*Note: ascii-chat is tested to work on Ubuntu 16.04 and macOS 10.12*

***Note: ascii-chat will NOT work on the CS 3110 Course VM, because it does not have the requisite webcam drivers!***

The provided install script can be used to install the necessary OPAM packages for building ascii-chat.

**Usage**: `./install.sh`

###OpenCV
OpenCV is required as a dependency in order to obtain images from the user's webcam. Scripts are provided for automated building and installation on Mac and Linux.

- To build OpenCV manually, use `./install-opencv-unix.sh`.

- To build OpenCV on Mac using Homebrew, use `./install-opencv-osx.sh`.
