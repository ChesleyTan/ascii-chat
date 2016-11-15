#include <stdio.h>
#include <opencv2/opencv.hpp>

extern "C" {
    void foo() {
        printf("Hello, I'm a shared library!\n");
        cv::VideoCapture cap(0);
        if (!cap.isOpened()) {
            fprintf(stderr, "Could not open default capture source!\n");
            return;
        }
        cv::Mat frame;
        cap >> frame;
        cv::imshow("orig", frame);
    }
}
