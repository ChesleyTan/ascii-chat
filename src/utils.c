#include <stdio.h>
#include <stdlib.h>
#include <sys/types.h>
#include <arpa/inet.h>
#include <net/if.h>
#include <ifaddrs.h>
#include "utils.h"

/* Adapted from CS 4410 - Project 3 */
char* get_addr_self() {
    struct ifaddrs *addr_list, *ifa;
    if (getifaddrs(&addr_list) < 0) {
        return NULL;
    }
    char *addr = NULL;
    for (ifa = addr_list; ifa != 0; ifa = ifa->ifa_next) {
        if (ifa->ifa_addr != 0 && ifa->ifa_addr->sa_family == AF_INET &&
                !(ifa->ifa_flags & IFF_LOOPBACK)) {
            struct sockaddr_in *si = (struct sockaddr_in *) ifa->ifa_addr;
            addr = inet_ntoa(si->sin_addr);
            break;
        }
    }
    freeifaddrs(addr_list);
    return addr;
}
