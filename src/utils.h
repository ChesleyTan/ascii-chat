/*
 * Utility functions for low level C calls
 */

/* Returns the ipv4 address of the computer's network interface
 * If the computer has multiple network interfaces, then the first one is chosen
 */
char* get_addr_self();
