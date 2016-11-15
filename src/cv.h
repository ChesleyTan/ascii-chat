#ifndef __CV_H__
#define __CV_H__

extern "C" {
    int frame_height();
    int frame_width();
    int frame_depth();
    unsigned char *read_frame();
}

#endif
