#!/bin/bash

OPENCV_VERSION=3.1.0

HELPFUL_GUIDES="
http://www.learnopencv.com/install-opencv-3-on-yosemite-osx-10-10-x/
http://www.pyimagesearch.com/2016/10/24/ubuntu-16-04-how-to-install-opencv/
"

# ANSI Escape Codes
RED="\033[1;38;5;9m"
GREEN="\033[1;38;5;10m"
YELLOW="\033[1;38;5;11m"
RESET="\033[m"

echo -e "${YELLOW}Installing opencv-$OPENCV_VERSION for Mac/Linux${RESET} ...\n"
echo -e "================================================================================
Make sure that you have the required dependencies!
Linux:
    sudo apt install git unzip build-essential cmake pkg-config libjpeg8-dev libtiff5-dev libjasper-dev libpng12-dev libavcodec-dev libavformat-dev libswscale-dev libv4l-dev libxvidcore-dev libx264-dev libgtk-3-dev libatlas-base-dev gfortran libdc1394-22 libdc1394-22-dev libtbb-dev
    #libxine-dev libgstreamer0.10-dev libgstreamer-plugins-base0.10-dev libqt4-dev libfaac-dev libmp3lame-dev libopencore-amrnb-dev libopencore-amrwb-dev libtheora-dev libvorbis-dev libxvidcore-dev x264

Mac:
    Install Xcode Command Line Tools: http://railsapps.github.io/xcode-command-line-tools.html
    Install Homebrew: http://brew.sh
    Install cmake: brew install cmake pkg-config
    Install image libraries: brew install jpeg libpng libtiff openexr
    Install optimization libraries: brew install eigen tbb
================================================================================
"
echo -e "${YELLOW}Continue install? [y|n]${RESET}"
read ans
if [[ ! $ans =~ ^[Yy]$ ]]; then
    exit
fi

cd /tmp
echo -e "${GREEN}Downloading opencv-$OPENCV_VERSION${RESET}"
wget -c https://github.com/Itseez/opencv/archive/$OPENCV_VERSION.zip
unzip $OPENCV_VERSION.zip
rm -f $OPENCV_VERSION.zip

echo -e "${GREEN}Building opencv-$OPENCV_VERSION${RESET}"
cd opencv-$OPENCV_VERSION
mkdir -p build
cd build
cmake -D CMAKE_BUILD_TYPE=RELEASE -D CMAKE_INSTALL_PREFIX=/usr/local -D WITH_TBB=ON -D WITH_V4L=ON .. &&
make -j4 &&
sudo make install &&
sudo ldconfig

if [[ $? = 0 ]]; then
    echo -e "${GREEN}Finished building opencv-$OPENCV_VERSION${RESET}"
else
    echo -e "${RED}There were errors building opencv-$OPENCV_VERSION :(${RESET}"
    echo -e "${RED}Try googling the errors produced above for solutions.${RESET}"
fi
