#include <stdio.h>
#include <opencv2/opencv.hpp>
#include <opencv2/core.hpp>
#include <opencv2/videoio.hpp>
#include <opencv2/imgproc.hpp>
#include <opencv2/highgui.hpp>

extern "C" {
    void foo() {
        printf("Hello, I'm a shared library!\n");
        cv::VideoCapture cap(0);
        if (!cap.isOpened()) {
            fprintf(stderr, "Could not open default capture source!\n");
            return;
        }
        cv::Mat frame;
        cv::namedWindow("orig", cv::WINDOW_AUTOSIZE);
        for (;;) {
            cap >> frame;
            cv::imshow("orig", frame);
            cv::waitKey(1);
        }
    }
}
