#include <stdio.h>
#include <opencv2/opencv.hpp>
#include <opencv2/core/core.hpp>
#include <opencv2/highgui/highgui.hpp>

extern "C" {
    unsigned char *data = NULL;
    int height = 0;
    int width = 0;
    int depth = 3; // r, g, b channels (we assume the depth is 3)

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
        cv::VideoCapture cap(0);
        if (!cap.isOpened()) {
            fprintf(stderr, "Could not open default capture source!\n");
            return NULL;
        }
        cv::Mat frame;
        cap >> frame;
        if (width != frame.size[0] || height != frame.size[1]) {
            width = frame.size[0];
            height = frame.size[1];
            //printf("%d x %d\n", height, width);
            if (data) {
                free(data);
            }
            data = (unsigned char *)malloc(sizeof(*data) * width * height * depth);
        }
        memcpy(data, frame.data, sizeof(*data) * width * height * depth);
        return data;
    }
}
