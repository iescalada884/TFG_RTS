// 	------------------------------------------------------------------- 
// 	-- 1) Functions to create the Messages that will be sent to the Daemon
// 	-------------------------------------------------------------------
// 	--		a) I2C_Flags
// 	-------------------------------------------------------------------
// 	--		The FLAGS may be useful in the future to add other features
// 	--		like supporting Ten Bit addresses, etc...
// 	-------------------------------------------------------------------
#include <sys/i2c.h> 
#include <stdlib.h> 
										
int flags_init (i2c_flags *flags)
{
	*flags = I2C_DEFAULT_FLAGS;
	return 0; 
}
int flags_set_tenbitaddress (i2c_flags *flags)
{
	*flags = *flags | I2C_M_TEN;
	return 0;
}
int flags_set_nostart (i2c_flags *flags)
{
	*flags = *flags | I2C_M_NOSTART;
	return 0;
}
int flags_set_revdiraddr (i2c_flags *flags)
{
	*flags = *flags | I2C_M_REV_DIR_ADDR;
	return 0;
}
int flags_set_ignorenack (i2c_flags *flags)
{
	*flags = *flags | I2C_M_IGNORE_NAK;
	return 0;
}
int flags_set_nordack (i2c_flags *flags)
{
	*flags = *flags | I2C_M_NO_RD_ACK;
	return 0;
}

int i2c_msg_c_init (i2c_c_msg *msg,  
						  i2c_address addr,
						  i2c_buffer buffer,	
						  i2c_flags flags)
{
	msg->addr = addr;
	msg->flags = flags;
	msg->buffer = buffer;
	return 0;	
}
// 							
// int i2c_msg_c_list_init(i2c_msg_c_list *msg_list, int msg_max)
// {
// 	msg_list->msg_list = (i2c_c_msg *)malloc(msg_max*sizeof(i2c_c_msg));
// 	msg_list->num = 0;
// 	msg_list->max_num = msg_max;
// 	return 0;	
// }
// 								
// int i2c_msg_c_list_destroy(i2c_msg_c_list *msg_list)
// {
//  	msg_list->num = 0;
// 	msg_list->max_num = 0;
// 	free(msg_list->msg_list);
// 	return 0;
// }								
// 								
// int i2c_msg_c_list_add (i2c_msg_c_list *msg_list, i2c_c_msg msg)
// {
// 	if ((msg_list->num + 1) <= msg_list->max_num){
// 		msg_list->msg_list[msg_list->num] = msg;
// 		msg_list->num = msg_list->num + 1;
// 	}	
// 	return 0;		
// }
