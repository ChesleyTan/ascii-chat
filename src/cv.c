#include <stdio.h>
#include <opencv2/opencv.hpp>
#include <opencv2/core.hpp>
#include <opencv2/videoio.hpp>
#include <opencv2/imgproc.hpp>
#include <opencv2/highgui.hpp>
#include <thread>
#include <atomic>

extern "C" {
    cv::VideoCapture *cap = NULL;
    unsigned char *data = NULL;
    int height = 0;
    int width = 0;
    int depth = 3; // b, g, r channels (we assume the depth is 3)
    std::atomic<bool> grabber_thread_running;

    int frame_height() {
        return height;
    }

    int frame_width() {
        return width;
    }

    int frame_depth() {
        return depth;
    }

    void grab_thread(cv::VideoCapture *cap) {
        // Continuously grab frames from camera to avoid retrieving stale frames
        // from internal buffer
        while (grabber_thread_running.load()) {
            if (cap->isOpened()) {
                cap->grab();
            }
        }
    }

    unsigned char *read_frame(int desired_width, int desired_height) {
        if (cap == NULL) {
            cap = new cv::VideoCapture(0);
            grabber_thread_running.store(true);
            // Workaround for unusual corrupted first frame bug
            if (cap->isOpened()) {
                cap->grab(); // Skip first frame
                cap->grab();
            }
            std::thread grabber(grab_thread, cap);
            grabber.detach();
        }
        if (!cap->isOpened()) {
            fprintf(stderr, "Could not open default capture source!\n");
            return NULL;
        }
        cv::Mat frame;
        if (!cap->retrieve(frame)) {
            *cap >> frame;
        }
        if (frame.size[0] != desired_height || frame.size[1] != desired_width) {
            cv::resize(frame, frame, cv::Size(desired_width, desired_height), 0, 0, cv::INTER_LINEAR);
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
        // Note: Race condition with grabber thread when calling cleanup could
        // result in segfault
        grabber_thread_running.store(false);
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
