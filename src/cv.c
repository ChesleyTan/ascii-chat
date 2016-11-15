#include <stdio.h>
#include <opencv2/opencv.hpp>
#include <opencv2/core/core.hpp>
#include <opencv2/highgui/highgui.hpp>

extern "C" {
    cv::VideoCapture *cap = NULL;
    unsigned char *data = NULL;
    int height = 0;
    int width = 0;
    int depth = 3; // r, g, b channels (we assume the depth is 3)
    int MAX_SIZE = 96;

    int frame_height() {
        return height;
    }

    int frame_width() {
        return width;
    }

    int frame_depth() {
        return depth;
    }

    unsigned char *read_frame() {
        if (cap == NULL) {
            cap = new cv::VideoCapture(0);
        }
        if (!cap->isOpened()) {
            fprintf(stderr, "Could not open default capture source!\n");
            return NULL;
        }
        cv::Mat frame;
        *cap >> frame;
        if (frame.size[0] > MAX_SIZE || frame.size[1] > MAX_SIZE) {
            cv::resize(frame, frame, cv::Size(MAX_SIZE, MAX_SIZE), 0, 0, CV_INTER_LINEAR);
        }
        if (height != frame.size[0] || width != frame.size[1]) {
            height = frame.size[0];
            width = frame.size[1];
            //printf("%d x %d\n", height, width);
            if (data) {
                free(data);
            }
            data = (unsigned char *)malloc(sizeof(*data) * width * height * depth);
        }
        memcpy(data, frame.data, sizeof(*data) * width * height * depth);
        return data;
    }

    void cleanup() {
        if (cap) {
            delete cap;
            cap = NULL;
        }
        if (data) {
            free(data);
            data = NULL;
        }
    }
}
