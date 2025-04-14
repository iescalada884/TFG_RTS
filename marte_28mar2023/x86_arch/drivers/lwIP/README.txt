/*
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 * 
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 * 
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
 * MA 02110-1301, USA.
 */

28.10.2013														v 0.1

This is the porting of lwIP to MaRTE OS. This is a beta version, not 
thoroughly tested and not designed to meet real-time requirements. It 
is based on lwIP v1.4.1 and RTL-lwIP v0.4.

It provides a full TCP/IP and UDP/IP stacks with additional protocols
like ARP (Address Resolution Protocol) for Ethernet. See README in src
directory for further details.


Files
---------

apps/     - The examples provided by lwIP. Not tested.

example/  - Example of using BSD-sockets with UDP/IP over MaRTE OS.

src/      - Source code of lwIP and porting to MaRTE OS.


How to run the example
------------------------
1. Configure MaRTE to use the Ethernet driver for your nic (see 
marte-kernel-devices_table.ads).

2. Configure your application in lwiopts.h. This file already includes 
most of the configurable options, but you could have a look to opt.h to
check all the parameters (e.g., thread priorities).

3. Enter in the example directory and run 'make' (mgcc should be in 
your $PATH)

4. Run the kernel executable (client and server). For example, to run it
in qemu:

>> qemu -net nic,macaddr=00:30:64:07:A2:64,model=rtl8139 -net socket,mcast=230.0.0.1:1234 -kernel client

>> qemu -net nic,macaddr=00:30:64:07:A2:62,model=rtl8139 -net socket,mcast=230.0.0.1:1234 -kernel server


Known issues
----------------
* There some minor fragmentation issues in this version.
* UDP checksum if not processed correctly.


TO-DO list
--------------------
* Development of Ada wrappers for GNAT.Sockets package.
* Adaptation to meet real-time constraints:
  - Remove the decoupling threading model used in this version.
  - Use of semaphores.
  - Use of TOS filed in the Ethernet driver (CoS is not supported yet).
  - Use of static ARP (this is partially supported in this version)
