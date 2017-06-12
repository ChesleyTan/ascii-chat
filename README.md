# ascii-chat

A terminal-based peer-to-peer (P2P) end-to-end-encrypted (E2EE) video chat application with text messaging and audio support.

## Installation

*Note: ascii-chat is tested to work on Ubuntu 16.04 and macOS 10.12*

***Note: ascii-chat will NOT work on the CS 3110 Course VM, because it does not have the requisite webcam drivers!***

The provided install script can be used to install the necessary OPAM packages for building ascii-chat.

**Usage**: `./install.sh`

### OpenCV

OpenCV is required as a dependency in order to obtain images from the user's webcam. Scripts are provided for automated building and installation on Mac and Linux.

- To build OpenCV manually, use `./install-opencv-unix.sh`.

- To build OpenCV on Mac using Homebrew, use `./install-opencv-osx.sh`.

## Usage

### As Host

To run ascii-chat as a participant for the chat, enter the command:

`./ascii-chat <encryption_key>`

where `encryption_key` is the same key as that used by all other participants.

### As Participant

To run ascii-chat as the host user for the chat, enter the command:

`./ascii-chat <encryption_key> -host <host_ip:host_port>`

where `encryption_key` is a pre-shared key between all participants, and `host_ip:host_port` is the network address of the host user.

##Images

![ascii-chat1](/images/ascii-chat1.gif)
![ascii-chat2](/images/ascii-chat2.gif)
