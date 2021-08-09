
#include <errno.h>

#include "hs_msg.h"

ssize_t hs_sendmsg(int sockfd, const struct msghdr *msg, int flags, int *err) {
    int i = sendmsg(sockfd, msg, flags);
    *err = errno;
    return i;
}

ssize_t hs_recvmsg(int sockfd, struct msghdr *msg, int flags, int *err) {
    int i = recvmsg(sockfd, msg, flags);
    *err = errno;
    return i;
}


// cmsghdr manipulation macros
struct cmsghdr *hs_cmsg_firsthdr(struct msghdr *msgh) {
  return CMSG_FIRSTHDR(msgh);
}

struct cmsghdr *hs_cmsg_nxthdr(struct msghdr *msgh, struct cmsghdr *cmsg) {
  return CMSG_NXTHDR(msgh, cmsg);
}

size_t hs_cmsg_space(size_t length) {
  return CMSG_SPACE(length);
}

size_t hs_cmsg_len(size_t length) {
  return CMSG_LEN(length);
}

unsigned char *hs_cmsg_data(struct cmsghdr *cmsg) {
  return CMSG_DATA(cmsg);
}
