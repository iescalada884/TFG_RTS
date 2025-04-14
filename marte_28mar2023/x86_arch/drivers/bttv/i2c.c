#include <compbttv.h>
#include <linux/i2c.h>
#include <linux/errno.h>
//#include <linux/completion.h>
#include "bttvp.h"
#include <string.h>

static LIST_HEAD(adapters);
static LIST_HEAD(drivers);


struct device legacy_bus = {
        .bus_id         = "legacy",
};

void *i2c_get_clientdata (struct i2c_client *dev)
{
	return dev_get_drvdata (&dev->dev);
}

void i2c_set_clientdata (struct i2c_client *dev, void *data)
{
	dev_set_drvdata (&dev->dev, data);
}




/* match always succeeds, as we want the probe() to tell if we really accept this match */
static int i2c_device_match(struct device *dev, struct device_driver *drv)
{
	return 1;
}

struct bus_type i2c_bus_type = {
	.name =		"i2c",
	.match =	i2c_device_match,
};

int i2c_master_send(struct i2c_client *client,const char *buf ,int count)
{
	int ret;
	struct i2c_adapter *adap=client->adapter;
	struct i2c_msg msg;

	if (client->adapter->algo->master_xfer) {
		msg.addr   = client->addr;
		msg.flags = client->flags & I2C_M_TEN;
		msg.len = count;
		msg.buf = (char *)buf;
	
		DEB2(dev_dbg3(&client->adapter->dev, "master_send: writing %d bytes.\n",
				count));
	
		//down(&adap->bus_lock);
		ret = adap->algo->master_xfer(adap,&msg,1);
		//up(&adap->bus_lock);

		/* if everything went ok (i.e. 1 msg transmitted), return #bytes
		 * transmitted, else error code.
		 */
		return (ret == 1 )? count : ret;
	} else {
		dev_err2(&client->adapter->dev, "I2C level transfers not supported\n");
		return -ENOSYS;
	}
}


int i2c_master_recv(struct i2c_client *client, char *buf ,int count)
{
	struct i2c_adapter *adap=client->adapter;
	struct i2c_msg msg;
	int ret;
	if (client->adapter->algo->master_xfer) {
		msg.addr   = client->addr;
		msg.flags = client->flags & I2C_M_TEN;
		msg.flags |= I2C_M_RD;
		msg.len = count;
		msg.buf = buf;

		DEB2(dev_dbg3(&client->adapter->dev, "master_recv: reading %d bytes.\n",
				count));
	
		//down(&adap->bus_lock);
		ret = adap->algo->master_xfer(adap,&msg,1);
		//up(&adap->bus_lock);
	
		DEB2(printk(KERN_DEBUG "i2c-core.o: master_recv: return:%d (count:%d, addr:0x%02x)\n",
			ret, count, client->addr));
	
		/* if everything went ok (i.e. 1 msg transmitted), return #bytes
	 	* transmitted, else error code.
	 	*/
		return (ret == 1 )? count : ret;
	} else {
		dev_err2(&client->adapter->dev, "I2C level transfers not supported\n");
		return -ENOSYS;
	}
}





int i2c_device_probe(struct device *dev)
{
	return -ENODEV;
}

int i2c_device_remove(struct device *dev)
{
	return 0;
}

static void i2c_adapter_dev_release(struct device *dev)
{
  /*	struct i2c_adapter *adap = dev_to_i2c_adapter(dev);
	complete(&adap->dev_released);*/
}
static struct device_driver i2c_adapter_driver = {
	.name =	"i2c_adapter",
	.bus = &i2c_bus_type,
	.probe = i2c_device_probe,
	.remove = i2c_device_remove,
};

static void i2c_adapter_class_dev_release(struct class_device *dev)
{
  /*	struct i2c_adapter *adap = class_dev_to_i2c_adapter(dev);
	complete(&adap->class_dev_released);*/
}

static struct class i2c_adapter_class = {
	.name =		"i2c-adapter",
	.release =	&i2c_adapter_class_dev_release,
};

/* ---------------------------------------------------
 * registering functions 
 * --------------------------------------------------- 
 */

/* -----
 * i2c_add_adapter is called from within the algorithm layer,
 * when a new hw adapter registers. A new device is register to be
 * available for clients.
 */
int i2c_add_adapter(struct i2c_adapter *adap)
{
	static int nr = 0;
	struct list_head   *item;
	struct i2c_driver  *driver;

	//down(&core_lists);

	adap->nr = nr++;
	list_add_tail(&adap->list,&adapters);
	INIT_LIST_HEAD(&adap->clients);

	/* Add the adapter to the driver core.
	 * If the parent pointer is not set up,
	 * we add this adapter to the legacy bus.
	 */
	if (adap->dev.parent == NULL)
		adap->dev.parent = &legacy_bus;
	sprintf26(adap->dev.bus_id, "i2c-%d", adap->nr);
	adap->dev.driver = &i2c_adapter_driver;
	adap->dev.release = &i2c_adapter_dev_release;
	/*MaRTE OS*/
	//	device_register(&adap->dev);
	//	device_create_file(&adap->dev, &dev_attr_name);

	/* Add this adapter to the i2c_adapter class */
	memset(&adap->class_dev, 0x00, sizeof(struct class_device));
	adap->class_dev.dev = &adap->dev;
	adap->class_dev.class = &i2c_adapter_class;
	strncpy(adap->class_dev.class_id, adap->dev.bus_id, BUS_ID_SIZE);
	/*MaRTE OS*/
	//	class_device_register(&adap->class_dev);

	/* inform drivers of new adapters */
	list_for_each(item,&drivers) {
		driver = list_entry(item, struct i2c_driver, list);
		if (driver->flags & I2C_DF_NOTIFY)
			/* We ignore the return code; if it fails, too bad */
			driver->attach_adapter(adap);
	}

	dev_dbg3(&adap->dev, "registered as adapter #%d\n", adap->nr);
	return 0;
}

int i2c_del_adapter(struct i2c_adapter *adap)
{
	struct list_head  *item, *_n;
	struct i2c_driver *driver;
	struct i2c_client *client;
	int res = 0;

	//down(&core_lists);

	list_for_each(item,&drivers) {
		driver = list_entry(item, struct i2c_driver, list);
		if (driver->detach_adapter)
			if ((res = driver->detach_adapter(adap))) {
				dev_warn3(&adap->dev, "can't detach adapter"
					 "while detaching driver %s: driver not "
					 "detached!", driver->name);
				goto out_unlock;
			}
	}

	/* detach any active clients. This must be done first, because
	 * it can fail; in which case we give upp. */
	list_for_each_safe(item, _n, &adap->clients) {
		client = list_entry(item, struct i2c_client, list);

		/* detaching devices is unconditional of the set notify
		 * flag, as _all_ clients that reside on the adapter
		 * must be deleted, as this would cause invalid states.
		 */
		if ((res=client->driver->detach_client(client))) {
			dev_err3(&adap->dev, "adapter not "
				"unregistered, because client at "
				"address %02x can't be detached. ",
				client->addr);
			goto out_unlock;
		}
	}

	/* clean up the sysfs representation */
	//	init_completion(&adap->dev_released);
	//	init_completion(&adap->class_dev_released);
	//	class_device_unregister(&adap->class_dev);
	//	device_remove_file(&adap->dev, &dev_attr_name);
	//	device_unregister(&adap->dev);
	list_del(&adap->list);

	/* wait for sysfs to drop all references */
	//	wait_for_completion(&adap->dev_released);
	//	wait_for_completion(&adap->class_dev_released);

	DEB(dev_dbg2(&adap->dev, "adapter unregistered\n"));

 out_unlock:
	//up(&core_lists);
	return res;
}


/* -----
 * What follows is the "upwards" interface: commands for talking to clients,
 * which implement the functions to access the physical information of the
 * chips.
 */

int i2c_add_driver(struct i2c_driver *driver)
{
	struct list_head   *item;
	struct i2c_adapter *adapter;
	int res = 0;

	//down(&core_lists);

	/* add the driver to the list of i2c drivers in the driver core */
	driver->driver.name = driver->name;
	driver->driver.bus = &i2c_bus_type;
	driver->driver.probe = i2c_device_probe;
	driver->driver.remove = i2c_device_remove;

	/*	res = driver_register(&driver->driver);
	if (res)
		goto out_unlock;
	*/

	list_add_tail(&driver->list,&drivers);
	DEB(printk(KERN_DEBUG "i2c-core.o: driver %s registered.\n",driver->name));

	/* now look for instances of driver on our adapters */
	if (driver->flags & I2C_DF_NOTIFY) {
		list_for_each(item,&adapters) {
			adapter = list_entry(item, struct i2c_adapter, list);
			driver->attach_adapter(adapter);
		}
	}

	// out_unlock:
	//up(&core_lists);
	return res;
}

static int i2cdev_command(struct i2c_client *client, unsigned int cmd,
                           void *arg)
{
	return -1;
}

static struct i2c_driver i2cdev_driver = {
  //	.owner		= THIS_MODULE,
	.name		= "dev_driver",
	.id		= I2C_DRIVERID_I2CDEV,
	.flags		= I2C_DF_NOTIFY,
	.attach_adapter	= NULL, //i2cdev_attach_adapter,
	.detach_adapter	= NULL, //i2cdev_detach_adapter,
	.detach_client	= NULL, //i2cdev_detach_client,
	.command	= i2cdev_command,
};

void i2c_fill_driver(struct i2c_client *client)
{
  client->driver=&i2cdev_driver;
}

void i2c_clients_command(struct i2c_adapter *adap, unsigned int cmd, void *arg)
{
  struct i2c_client *client;
  struct device dev;
  struct bttv *btv;

  dev=adap->dev;
  btv=(struct bttv *)dev.driver_data;
  client=&btv->i2c_client;

  if (NULL != client->driver->command) {
    client->driver->command(client,cmd,arg);
  }
}

int i2c_dev_init(void){
  
  i2cdev_driver.driver.name="dev_driver";
  i2cdev_driver.driver.bus=&i2c_bus_type;
  i2cdev_driver.driver.probe=&i2c_device_probe;
  i2cdev_driver.driver.remove=&i2c_device_remove;

  return 1;
}
