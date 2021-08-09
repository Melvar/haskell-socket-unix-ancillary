
#include <sys/socket.h>

ssize_t hs_sendmsg(int sockfd, const struct msghdr *msg, int flags, int *err);

ssize_t hs_recvmsg(int sockfd, struct msghdr *msg, int flags, int *err);


// cmsghdr manipulation macros
struct cmsghdr *hs_cmsg_firsthdr(struct msghdr *msgh);

struct cmsghdr *hs_cmsg_nxthdr(struct msghdr *msgh, struct cmsghdr *cmsg);

size_t hs_cmsg_space(size_t length);

size_t hs_cmsg_len(size_t length);

unsigned char *hs_cmsg_data(struct cmsghdr *cmsg);

