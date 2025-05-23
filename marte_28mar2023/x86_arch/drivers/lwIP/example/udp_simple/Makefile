#
# Copyright (c) 2001, 2002 Swedish Institute of Computer Science.
# All rights reserved. 
# 
# Redistribution and use in source and binary forms, with or without modification, 
# are permitted provided that the following conditions are met:
#
# 1. Redistributions of source code must retain the above copyright notice,
#    this list of conditions and the following disclaimer.
# 2. Redistributions in binary form must reproduce the above copyright notice,
#    this list of conditions and the following disclaimer in the documentation
#    and/or other materials provided with the distribution.
# 3. The name of the author may not be used to endorse or promote products
#    derived from this software without specific prior written permission. 
#
# THIS SOFTWARE IS PROVIDED BY THE AUTHOR ``AS IS'' AND ANY EXPRESS OR IMPLIED 
# WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF 
# MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT 
# SHALL THE AUTHOR BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, 
# EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT 
# OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS 
# INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN 
# CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING 
# IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY 
# OF SUCH DAMAGE.
#
# This file is part of the lwIP TCP/IP stack.
# 
# Author: Adam Dunkels <adam@sics.se>
#
 
CCDEP=gcc #This is for dependences only
CC=mgcc
#To compile for linux: make ARCH=linux
#To compile for cygwin: make ARCH=cygwin
#To compile for openbsd: make ARCH=openbsd
ARCH=linux
CFLAGS= -g -Wall -D$(ARCH) -DIPv4 -O0 -fpack-struct -DLWIP_DEBUG
LDFLAGS=
ARFLAGS=rs

CONTRIBDIR=../../
LWIPARCH=$(CONTRIBDIR)/src/arch/marte

#Set this to where you have the lwip core module checked out from CVS
#default assumes it's a dir named lwip at the same level as the contrib module
LWIPDIR=../../src

CFLAGS:=$(CFLAGS) \
	-I$(LWIPDIR)/include -I$(LWIPARCH)/include -I$(LWIPDIR)/include/ipv4 \
	-I. -I$(CONTRIBDIR)/apps/snmp_private_mib -I$(LWIPDIR)

# COREFILES, CORE4FILES: The minimum set of files needed for lwIP.
COREFILES=$(LWIPDIR)/core/mem.c $(LWIPDIR)/core/memp.c $(LWIPDIR)/core/netif.c \
	$(LWIPDIR)/core/pbuf.c $(LWIPDIR)/core/raw.c \
	$(LWIPDIR)/core/stats.c $(LWIPDIR)/core/sys.c \
        $(LWIPDIR)/core/tcp.c $(LWIPDIR)/core/tcp_in.c \
        $(LWIPDIR)/core/tcp_out.c $(LWIPDIR)/core/udp.c $(LWIPDIR)/core/dhcp.c \
	$(LWIPDIR)/core/init.c $(LWIPDIR)/core/timers.c $(LWIPDIR)/core/def.c
CORE4FILES=$(wildcard $(LWIPDIR)/core/ipv4/*.c) $(LWIPDIR)/core/ipv4/inet.c \
	$(LWIPDIR)/core/ipv4/inet_chksum.c

# SNMPFILES: Extra SNMPv1 agent
SNMPFILES=$(LWIPDIR)/core/snmp/asn1_dec.c $(LWIPDIR)/core/snmp/asn1_enc.c \
	$(LWIPDIR)/core/snmp/mib2.c $(LWIPDIR)/core/snmp/mib_structs.c \
	$(LWIPDIR)/core/snmp/msg_in.c $(LWIPDIR)/core/snmp/msg_out.c \
	$(CONTRIBDIR)/apps/snmp_private_mib/lwip_prvmib.c

# APIFILES: The files which implement the sequential and socket APIs.
APIFILES=$(LWIPDIR)/api/api_lib.c $(LWIPDIR)/api/api_msg.c $(LWIPDIR)/api/tcpip.c \
	$(LWIPDIR)/api/err.c $(LWIPDIR)/api/sockets.c $(LWIPDIR)/api/netbuf.c $(LWIPDIR)/api/netdb.c

# NETIFFILES: Files implementing various generic network interface functions.'
NETIFFILES=$(LWIPDIR)/netif/etharp.c

# ARCHFILES: Architecture specific files
ARCHFILES=$(LWIPARCH)/sys_arch.c $(LWIPARCH)/netif/eth0_if.c

# LWIPFILES: All the above.
LWIPFILES=$(COREFILES) $(CORE4FILES) $(SNMPFILES) $(APIFILES) $(NETIFFILES) $(ARCHFILES)
LWIPFILESW=$(wildcard $(LWIPFILES))
LWIPOBJS=$(notdir $(LWIPFILESW:.c=.o))

# APPFILES
APPFILES=server.c client.c payload.c

LWIPLIB=liblwip4.a
APPLIB=liblwipapps.a
APPOBJS=$(notdir $(APPFILES:.c=.o))

%.o:
	$(CC) $(CFLAGS) -c $(<:.o=.c)

all ipv4 compile: client server
.PHONY: all

clean:
	rm -f *.o $(LWIPLIB) $(APPLIB) .depend* client server

depend dep: .depend

include .depend	

$(APPLIB): $(APPOBJS)
	$(AR) $(ARFLAGS) $(APPLIB) $?

$(LWIPLIB): $(LWIPOBJS)
	$(AR) $(ARFLAGS) $(LWIPLIB) $?

.depend: payload.c $(LWIPFILES) $(APPFILES)
	$(CCDEP) $(CFLAGS) -MM $^ > .depend || rm -f .depend

server:	.depend $(LWIPLIB) $(APPLIB) $(APPFILES)
	$(CC) $(CFLAGS) $(LDFLAGS) -o server server.c $(LWIPLIB)
	
client:	.depend $(LWIPLIB) $(APPLIB) $(APPFILES)
	$(CC) $(CFLAGS) $(LDFLAGS) -o client client.c payload.o $(LWIPLIB)



