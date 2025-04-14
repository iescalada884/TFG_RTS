// --------------------------------------------
// Module: rt61pci
// Abstract: rt61pci device specific routines.
// Supported chipsets: RT2561
// --------------------------------------------
//  INDEX:
//  1) GLOBAL VARIABLES
//  2) CONTROL-STATUS-REGISTERS ACCESS
//  3) TX/RX Descriptor access functions
//  4) EEPROM ACCESS
//  5) LEDs
//  6) CONFIGURATION FUNCTIONS
//  7) INTERRUPT FUNCTIONS
//  8) INITIALIZATION FUNCTIONS
//  9) TX FUNCTIONS
//  10) APPLICATION INTERFACE FUNCTIONS
// --------------------------------------------
#include "rt61.h"

// #define RT61_ENABLE_TIME_MEASUREMENTS
#ifdef RT61_ENABLE_TIME_MEASUREMENTS
#include <misc/time_measurement_posix.h>
#include <assert.h>
#endif

// --------------------------------------------
//  1) GLOBAL VARIABLES
// --------------------------------------------
mac_address the_mac_address;
struct rt2x00_dev the_rt2x00_dev;
sem_t irq_semaphore;
wifi_ring_t the_rx_ring_buffer;
wifi_frame_t the_tx_frame;

#ifdef RT61_ENABLE_TIME_MEASUREMENTS
time_measure_id_t rt61_measurement_id;
#endif

// --------------------------------------------
//  2) CONTROL-STATUS-REGISTERS ACCESS
// --------------------------------------------
// All access to the CSR registers will go through the methods
// rt2x00_register_read and rt2x00_register_write.
// BBP and RF register require indirect register access,
// and use the CSR registers PHY_CSR3 and PHY_CSR4 to achieve this.
// These indirect registers work with busy bits,
// and we will try maximal REGISTER_BUSY_COUNT times to access
// the register while taking a REGISTER_BUSY_DELAY us delay
// between each attampt. When the busy bit is still set at that time,
// the access attempt is considered to have failed,
// and we will print an error.
// The caller to these register access functions, should take precautions
// for the correct byte ordering of the values.
// -----------------------------------------------------------------------
static inline void rt2x00_register_read(
      const struct rt2x00_dev *rt2x00dev,
      const unsigned long offset, u32 *value)
{
   readl((void*)(rt2x00dev->csr_addr + MAC_CSR0));
   *value = readl((void*)(rt2x00dev->csr_addr + offset));
}
// -----------------------------------------------------------------------
static inline void rt2x00_register_multiread(
      const struct rt2x00_dev *rt2x00dev,
      const unsigned long offset, u32 *value, const u16 length)
{
   readl((void*)(rt2x00dev->csr_addr + MAC_CSR0));
   memcpy_fromio(
         (void*)value, (void*)(rt2x00dev->csr_addr + offset), length);
}
// -----------------------------------------------------------------------
static inline void rt2x00_register_write(
      const struct rt2x00_dev *rt2x00dev,
      const unsigned long offset, const u32 value)
{
   readl((void*)(rt2x00dev->csr_addr + MAC_CSR0));
   writel(value, (void*)(rt2x00dev->csr_addr + offset));
}
// -----------------------------------------------------------------------
static inline void rt2x00_register_multiwrite(
      const struct rt2x00_dev *rt2x00dev,
      const unsigned long offset, u32 *value, const u16 length)
{
   readl((void*)(rt2x00dev->csr_addr + MAC_CSR0));
   memcpy_toio(
         (void*)(rt2x00dev->csr_addr + offset), (void*)value, length);
}
// -----------------------------------------------------------------------
static void rt2x00_bbp_write(const struct rt2x00_dev *rt2x00dev,
                             const u8 reg_id, const u8 value)
{
   u32 reg;
   unsigned int i;

   for (i = 0; i < REGISTER_BUSY_COUNT; i++) {
      rt2x00_register_read(rt2x00dev, PHY_CSR3, &reg);
      if (!rt2x00_get_field32(reg, PHY_CSR3_BUSY))
         goto bbp_write;
      udelay(REGISTER_BUSY_DELAY);
   }

   ERROR("PHY_CSR3 register busy. Write failed.\n");
   return;

bbp_write:
      reg = 0;
      rt2x00_set_field32(&reg, PHY_CSR3_VALUE, value);
      rt2x00_set_field32(&reg, PHY_CSR3_REGNUM, reg_id);
      rt2x00_set_field32(&reg, PHY_CSR3_BUSY, 1);
      rt2x00_set_field32(&reg, PHY_CSR3_READ_CONTROL, 0);
      rt2x00_register_write(rt2x00dev, PHY_CSR3, reg);
}
// -----------------------------------------------------------------------
static void rt2x00_bbp_read(const struct rt2x00_dev *rt2x00dev,
                            const u8 reg_id, u8 *value)
{
   u32 reg;
   unsigned int i;

   // First request the register we wish to read from.
   reg =0;
   rt2x00_set_field32(&reg, PHY_CSR3_REGNUM, reg_id);
   rt2x00_set_field32(&reg, PHY_CSR3_BUSY, 1);
   rt2x00_set_field32(&reg, PHY_CSR3_READ_CONTROL, 1);

   rt2x00_register_write(rt2x00dev, PHY_CSR3, reg);

   for (i = 0; i < REGISTER_BUSY_COUNT; i++) {
      rt2x00_register_read(rt2x00dev, PHY_CSR3, &reg);
      if (!rt2x00_get_field32(reg, PHY_CSR3_BUSY)) {
         *value = rt2x00_get_field32(reg, PHY_CSR3_VALUE);
         return;
      }
      udelay(REGISTER_BUSY_DELAY);
   }

   ERROR("PHY_CSR3 register busy. Read failed.\n");
   *value = 0xff;
}
// -----------------------------------------------------------------------
static void rt2x00_rf_write(const struct rt2x00_dev *rt2x00dev,
                            const u32 value)
{
   u32 reg;
   unsigned int i;

   for (i = 0; i < REGISTER_BUSY_COUNT; i++) {
      rt2x00_register_read(rt2x00dev, PHY_CSR4, &reg);
      if (!rt2x00_get_field32(reg, PHY_CSR4_BUSY))
         goto rf_write;
      udelay(REGISTER_BUSY_DELAY);
   }

   ERROR("RFCSR register busy. Write failed.\n");
   return;

rf_write:
      reg = 0;
      rt2x00_set_field32(&reg, PHY_CSR4_VALUE, value);
      rt2x00_set_field32(&reg, PHY_CSR4_NUMBER_OF_BITS, 21);
      rt2x00_set_field32(&reg, PHY_CSR4_IF_SELECT, 0);
      rt2x00_set_field32(&reg, PHY_CSR4_BUSY, 1);
      rt2x00_register_write(rt2x00dev, PHY_CSR4, reg);
}
// -----------------------------------------------------------------------
static void rt2x00_mcu_request(const struct rt2x00_dev *rt2x00dev,
                               const u8 command, const u8 token, const u8 arg0, const u8 arg1)
{
   u32 reg;

   rt2x00_register_read(rt2x00dev, H2M_MAILBOX_CSR, &reg);

   if (rt2x00_get_field32(reg, H2M_MAILBOX_CSR_OWNER)) {
      ERROR("mcu request error. Request 0x%02x failed for "
            "token 0x%02x.\n", command, token);
      return;
   }

   rt2x00_set_field32(&reg, H2M_MAILBOX_CSR_OWNER, 1);
   rt2x00_set_field32(&reg, H2M_MAILBOX_CSR_CMD_TOKEN, token);
   rt2x00_set_field32(&reg, H2M_MAILBOX_CSR_ARG0, arg0);
   rt2x00_set_field32(&reg, H2M_MAILBOX_CSR_ARG1, arg1);
   rt2x00_register_write(rt2x00dev, H2M_MAILBOX_CSR, reg);

   rt2x00_register_read(rt2x00dev, HOST_CMD_CSR, &reg);
   rt2x00_set_field32(&reg, HOST_CMD_CSR_HOST_COMMAND, command);
   rt2x00_set_field32(&reg, HOST_CMD_CSR_INTERRUPT_MCU, 1);
   rt2x00_register_write(rt2x00dev, HOST_CMD_CSR, reg);
}
// --------------------------------------------
//  3) TX/RX Descriptor access functions
// --------------------------------------------
// Descriptors will be DMAed to/from MAC block to control the transmission
// behavior of the current frame.
// -----------------------------------------------------------------------
static inline void* rt2x00pci_data_addr(struct data_entry *entry)
{
   return entry->data_addr;
}
// -----------------------------------------------------------------------
static inline void* rt2x00pci_desc_addr(struct data_entry *entry)
{
   return entry->priv;
}
// -----------------------------------------------------------------------
static inline void rt2x00_desc_read(const void *desc,
                                    const u8 word, u32 *value)
{
   *value = le32_to_cpu(((__le32*)desc)[word]);
}
// -----------------------------------------------------------------------
static inline void rt2x00_desc_write(const void *desc,
                                     const u8 word, const u32 value)
{
   ((__le32*)desc)[word] = cpu_to_le32(value);
}
// --------------------------------------------
//  4) EEPROM ACCESS
// --------------------------------------------
// The EEPROM is being accessed by word index.
// rt2x00_eeprom_read is the main access function,
// which should be called from the rest of the module.
// It will take the index number of the eeprom word
// and the value in which the data should be stored.
// This function does assume the bus_width attribute
// within rt2x00_dev has been correctly set.
// -----------------------------------------------------------------------
static inline void rt2x00_eeprom_pulse_high(const struct rt2x00_dev *rt2x00dev,
                                            u32 *flags)
{
   rt2x00_set_field32(flags, E2PROM_CSR_DATA_CLOCK, 1);
   rt2x00_register_write(rt2x00dev, E2PROM_CSR, *flags);
   udelay(1);
}
// -----------------------------------------------------------------------
static inline void rt2x00_eeprom_pulse_low(const struct rt2x00_dev *rt2x00dev,
                                           u32 *flags)
{
   rt2x00_set_field32(flags, E2PROM_CSR_DATA_CLOCK, 0);
   rt2x00_register_write(rt2x00dev, E2PROM_CSR, *flags);
   udelay(1);
}
// -----------------------------------------------------------------------
static void rt2x00_eeprom_shift_out_bits(const struct rt2x00_dev *rt2x00dev,
                                         const u16 data, const u16 count)
{
   u32 flags;
   u32 mask =  1 << (count - 1);

   rt2x00_register_read(rt2x00dev, E2PROM_CSR, &flags);

   // Clear data flags.
   rt2x00_set_field32(&flags, E2PROM_CSR_DATA_IN, 0);
   rt2x00_set_field32(&flags, E2PROM_CSR_DATA_OUT, 0);
   // Start writing all bits.
   do {
      // Set the data_in bit only when required.
      if (data & mask)
         rt2x00_set_field32(&flags, E2PROM_CSR_DATA_IN, 1);
      else
         rt2x00_set_field32(&flags, E2PROM_CSR_DATA_IN, 0);

      rt2x00_register_write(rt2x00dev, E2PROM_CSR, flags);

      rt2x00_eeprom_pulse_high(rt2x00dev, &flags);
      rt2x00_eeprom_pulse_low(rt2x00dev, &flags);
      // Shift to next bit.
      mask >>= 1;
   } while (mask);

   rt2x00_set_field32(&flags, E2PROM_CSR_DATA_IN, 0);
   rt2x00_register_write(rt2x00dev, E2PROM_CSR, flags);
}
// -----------------------------------------------------------------------
static void rt2x00_eeprom_shift_in_bits(const struct rt2x00_dev *rt2x00dev,
                                        u16 *data)
{
   u32 flags;
   unsigned int i;

   rt2x00_register_read(rt2x00dev, E2PROM_CSR, &flags);
   // Clear data flags.
   rt2x00_set_field32(&flags, E2PROM_CSR_DATA_IN, 0);
   rt2x00_set_field32(&flags, E2PROM_CSR_DATA_OUT, 0);
   // Start reading all 16 bits.
   for (i = 0; i < 16; i++) {
      // Shift to the next bit.
      *data <<= 1;

      rt2x00_eeprom_pulse_high(rt2x00dev, &flags);

      rt2x00_register_read(rt2x00dev, E2PROM_CSR, &flags);
      // Clear data_in flag.
      rt2x00_set_field32(&flags, E2PROM_CSR_DATA_IN, 0);
      // Set the data bit to 1 when the data_out flag is set.
      if (rt2x00_get_field32(flags, E2PROM_CSR_DATA_OUT))
         *data |= 1;

      rt2x00_eeprom_pulse_low(rt2x00dev, &flags);
   }
}
// -----------------------------------------------------------------------
static void rt2x00_eeprom_read(const struct rt2x00_dev *rt2x00dev,
                               const u8 word, u16 *data)
{
   u32 flags;

   // Clear all flags, and enable chip select.
   rt2x00_register_read(rt2x00dev, E2PROM_CSR, &flags);
   rt2x00_set_field32(&flags, E2PROM_CSR_DATA_IN, 0);
   rt2x00_set_field32(&flags, E2PROM_CSR_DATA_OUT, 0);
   rt2x00_set_field32(&flags, E2PROM_CSR_DATA_CLOCK, 0);
   rt2x00_set_field32(&flags, E2PROM_CSR_CHIP_SELECT, 1);
   rt2x00_register_write(rt2x00dev, E2PROM_CSR, flags);

   // kick a pulse.
   rt2x00_eeprom_pulse_high(rt2x00dev, &flags);
   rt2x00_eeprom_pulse_low(rt2x00dev, &flags);

   // Select the read opcode and bus_width.
   rt2x00_eeprom_shift_out_bits(rt2x00dev, EEPROM_READ_OPCODE, 3);
   rt2x00_eeprom_shift_out_bits(rt2x00dev, word, rt2x00dev->eeprom_width);

   rt2x00_eeprom_shift_in_bits(rt2x00dev, data);

   // Clear chip_select and data_in flags.
   rt2x00_register_read(rt2x00dev, E2PROM_CSR, &flags);
   rt2x00_set_field32(&flags, E2PROM_CSR_DATA_IN, 0);
   rt2x00_set_field32(&flags, E2PROM_CSR_CHIP_SELECT, 0);
   rt2x00_register_write(rt2x00dev, E2PROM_CSR, flags);

   // kick a pulse.
   rt2x00_eeprom_pulse_high(rt2x00dev, &flags);
   rt2x00_eeprom_pulse_low(rt2x00dev, &flags);
}
// -----------------------------------------------------------------------
static void rt2x00_eeprom_multiread(const struct rt2x00_dev *rt2x00dev,
                                    const u8 word, u16 *data, const u16 length)
{
   unsigned int i;

   for (i = 0; i < (length / sizeof(u16)); i++)
      rt2x00_eeprom_read(rt2x00dev, word + i, data++);
}
// --------------------------------------------
//  5) LEDs
// --------------------------------------------
// Functions for enabling and disabling LEDs
// -----------------------------------------------------------------------
static void rt61pci_enable_led(struct rt2x00_dev *rt2x00dev)
{
   u32 reg;
   u16 led_reg;
   u8 arg0;
   u8 arg1;

   rt2x00_register_read(rt2x00dev, MAC_CSR14, &reg);
   rt2x00_set_field32(&reg, MAC_CSR14_ON_PERIOD, 70);
   rt2x00_set_field32(&reg, MAC_CSR14_OFF_PERIOD, 30);
   rt2x00_register_write(rt2x00dev, MAC_CSR14, reg);

   led_reg = rt2x00dev->led_reg;
   rt2x00_set_field16(&led_reg, MCU_LEDCS_RADIO_STATUS, 1);
   rt2x00_set_field16(&led_reg, MCU_LEDCS_LINK_BG_STATUS, 1);

   arg0 = led_reg & 0xff;
   arg1 = (led_reg >> 8) & 0xff;

   rt2x00_mcu_request(rt2x00dev, MCU_LED, 0xff, arg0, arg1);
}
// -----------------------------------------------------------------------
static void rt61pci_disable_led(struct rt2x00_dev *rt2x00dev)
{
   u16 led_reg;
   u8 arg0;
   u8 arg1;

   led_reg = rt2x00dev->led_reg;
   rt2x00_set_field16(&led_reg, MCU_LEDCS_RADIO_STATUS, 0);
   rt2x00_set_field16(&led_reg, MCU_LEDCS_LINK_BG_STATUS, 0);
   rt2x00_set_field16(&led_reg, MCU_LEDCS_LINK_A_STATUS, 0);

   arg0 = led_reg & 0xff;
   arg1 = (led_reg >> 8) & 0xff;

   rt2x00_mcu_request(rt2x00dev, MCU_LED, 0xff, arg0, arg1);
}
// --------------------------------------------
//  6) CONFIGURATION FUNCTIONS
// --------------------------------------------
static void rt61pci_config_mac_address(struct rt2x00_dev *rt2x00dev,
                                       void *addr)
{
   u32 reg[2] = { 0, 0 };

   // The MAC address is passed to us as an array of bytes,
   // that array is little endian, so no need for byte ordering.
   // We only need to set the MAC_CSR3_UNICAST_TO_ME_MASK
   // at the correct offset.
   memcpy(&reg, addr, 6);
   rt2x00_set_field32(&reg[1], MAC_CSR3_UNICAST_TO_ME_MASK, 0xff);
   rt2x00_register_multiwrite(rt2x00dev, MAC_CSR2, &reg[0], sizeof(reg));
}
// -----------------------------------------------------------------------
static void rt61pci_config_promisc(struct rt2x00_dev *rt2x00dev, int promisc)
{
   u32 reg;

   rt2x00_register_read(rt2x00dev, TXRX_CSR0, &reg);

   if (promisc) {
      rt2x00_set_field32(&reg, TXRX_CSR0_DROP_NOT_TO_ME, 0);
      SET_FLAG(rt2x00dev, INTERFACE_ENABLED_PROMISC);
   } else {
      rt2x00_set_field32(&reg, TXRX_CSR0_DROP_NOT_TO_ME, 1);
      CLEAR_FLAG(rt2x00dev, INTERFACE_ENABLED_PROMISC);
   }

   rt2x00_register_write(rt2x00dev, TXRX_CSR0, reg);
}
// -----------------------------------------------------------------------
static void rt61pci_config_channel(struct rt2x00_dev *rt2x00dev,
                                   int rf2, int channel, int freq, int txpower)
{
   u8 reg = 0;
   u32 rf1 = 0;
   u32 rf3 = 0;
   u32 rf4 = 0;

   if (txpower == 0xff)
      txpower = rt2x00dev->tx_power;
   txpower = TXPOWER_TO_DEV(txpower);

   if (!GET_FLAG(rt2x00dev, CONFIG_RF_SEQUENCE) || channel <= 14)
      rf1 = cpu_to_le32(0x00002ccc);
   else if (channel == 36 ||
            (channel >= 100 && channel <= 116) ||
            channel >= 157)
      rf1 = cpu_to_le32(0x00002cd4);
   else
      rf1 = cpu_to_le32(0x00002cd0);

   if (channel <= 14) {
      rf3 = cpu_to_le32(0x00068455);
   } else if (!GET_FLAG(rt2x00dev, CONFIG_RF_SEQUENCE)) {
      if (channel >= 36 && channel <= 48)
         rf3 = cpu_to_le32(0x0009be55);
      else if (channel >= 52 && channel <= 64)
         rf3 = cpu_to_le32(0x0009ae55);
      else if (channel >= 100 && channel <= 112)
         rf3 = cpu_to_le32(0x000bae55);
      else
         rf3 = cpu_to_le32(0x000bbe55);
   } else {
      switch (channel) {
         case 36:
         case 40:
         case 44:
            rf3 = cpu_to_le32(0x00098455);
            break;
         case 48:
            rf3 = cpu_to_le32(0x00098655);
            break;
         case 52:
            rf3 = cpu_to_le32(0x00098855);
            break;
         case 56:
            rf3 = cpu_to_le32(0x00098c55);

         case 60:
            rf3 = cpu_to_le32(0x00098e55);
            break;
         case 64:
            rf3 = cpu_to_le32(0x00099255);
            break;
         case 100:
         case 104:
         case 108:
            rf3 = cpu_to_le32(0x000b9855);
            break;
         case 112:
         case 116:
         case 120:
         case 124:
            rf3 = cpu_to_le32(0x000b9a55);
            break;
         case 128:
         case 132:
            rf3 = cpu_to_le32(0x000b9c55);
            break;
         case 136:
         case 140:
            rf3 = cpu_to_le32(0x000b9e55);
            break;
         case 149:
         case 153:
         case 157:
         case 161:
         case 165:
            rf3 = cpu_to_le32(0x000ba255);
            break;
      }
   }

   if (channel < 14) {
      if (channel & 1)
         rf4 = cpu_to_le32(0x000ffa0b);
      else
         rf4 = cpu_to_le32(0x000ffa1f);
   } else if (channel == 14) {
      rf4 = cpu_to_le32(0x000ffa13);
   } else if (!GET_FLAG(rt2x00dev, CONFIG_RF_SEQUENCE)) {
      switch (channel) {
         case 36:
         case 56:
         case 116:
         case 136:
            rf4 = cpu_to_le32(0x000ffa23);
            break;
         case 40:
         case 60:
         case 100:
         case 120:
         case 140:
            rf4 = cpu_to_le32(0x000ffa03);
            break;
         case 44:
         case 64:
         case 104:
         case 124:
            rf4 = cpu_to_le32(0x000ffa0b);
            break;
         case 48:
         case 108:
         case 128:
            rf4 = cpu_to_le32(0x000ffa13);
            break;
         case 52:
         case 112:
         case 132:
            rf4 = cpu_to_le32(0x000ffa1b);
            break;
         case 149:
            rf4 = cpu_to_le32(0x000ffa1f);
            break;
         case 153:
            rf4 = cpu_to_le32(0x000ffa27);
            break;
         case 157:
            rf4 = cpu_to_le32(0x000ffa07);
            break;
         case 161:
            rf4 = cpu_to_le32(0x000ffa0f);
            break;
         case 165:
            rf4 = cpu_to_le32(0x000ffa17);
            break;
      }
   } else {
      switch (channel) {
         case 36:
         case 40:
         case 60:
         case 140:
         case 100:
         case 104:
         case 108:
         case 112:
         case 116:
         case 120:
            rf4 = cpu_to_le32(0x000c0a03);
            break;
         case 44:
         case 64:
         case 124:
         case 149:
            rf4 = cpu_to_le32(0x000c0a1b);
            break;
         case 48:
         case 128:
         case 153:
            rf4 = cpu_to_le32(0x000c0a0b);
            break;
         case 52:
         case 132:
            rf4 = cpu_to_le32(0x000c0a23);
            break;
         case 56:
         case 136:
            rf4 = cpu_to_le32(0x000c0a13);
            break;
         case 157:
         case 161:
         case 165:
            rf4 = cpu_to_le32(0x000c0a17);
            break;
      }
   }

   // Set TXpower.
   rt2x00_set_field32(&rf3, RF3_TXPOWER, txpower);

   INFO("Switching channel. RF1: 0x%08x, RF2: 0x%08x, RF3: 0x%08x, "
         "RF4: 0x%08x.\n", rf1, rf2, rf3, rf4);

   // Set Frequency offset.
   rt2x00_set_field32(&rf4, RF4_FREQ_OFFSET, rt2x00dev->freq_offset);

   rt2x00_rf_write(rt2x00dev, rf1);
   rt2x00_rf_write(rt2x00dev, rf2);
   rt2x00_rf_write(rt2x00dev, rf3 & ~cpu_to_le32(0x00000004));
   rt2x00_rf_write(rt2x00dev, rf4);

   udelay(200);

   rt2x00_rf_write(rt2x00dev, rf1);
   rt2x00_rf_write(rt2x00dev, rf2);
   rt2x00_rf_write(rt2x00dev, rf3 | cpu_to_le32(0x00000004));
   rt2x00_rf_write(rt2x00dev, rf4);

   udelay(200);

   rt2x00_rf_write(rt2x00dev, rf1);
   rt2x00_rf_write(rt2x00dev, rf2);
   rt2x00_rf_write(rt2x00dev, rf3 & ~cpu_to_le32(0x00000004));
   rt2x00_rf_write(rt2x00dev, rf4);

   rt2x00_bbp_read(rt2x00dev, 3, &reg);
   if (rt2x00_rf(&rt2x00dev->chip, RF5225) ||
       rt2x00_rf(&rt2x00dev->chip, RF2527))
      reg &= ~0x01;
   else
      reg |= 0x01;
   rt2x00_bbp_write(rt2x00dev, 3, reg);

   msleep(1);

   rt2x00dev->tx_power = txpower;

   // Update rf fields
   rt2x00dev->rf1 = rf1;
   rt2x00dev->rf2 = rf2;
   rt2x00dev->rf3 = rf3;
   rt2x00dev->rf4 = rf4;
}
// -----------------------------------------------------------------------
static void rt61pci_config_filter_sync(struct rt2x00_dev *rt2x00dev)
{
   u32 reg;

   rt2x00_register_write(rt2x00dev, TXRX_CSR9, 0);
   // Apply hardware packet filter (more on this in rt61.h)
   rt2x00_register_read(rt2x00dev, TXRX_CSR0, &reg);
   rt2x00_set_field32(&reg, TXRX_CSR0_DROP_TO_DS, 1);
   rt2x00_set_field32(&reg, TXRX_CSR0_DROP_CRC, 1);
   rt2x00_set_field32(&reg, TXRX_CSR0_DROP_PHYSICAL, 1);
   rt2x00_set_field32(&reg, TXRX_CSR0_DROP_CONTROL, 1);
   rt2x00_set_field32(&reg, TXRX_CSR0_DROP_VERSION_ERROR, 1);
   rt2x00_set_field32(&reg, TXRX_CSR0_DROP_NOT_TO_ME, 1);
   rt2x00_set_field32(&reg, TXRX_CSR0_DROP_MULTICAST, 0);
   rt2x00_set_field32(&reg, TXRX_CSR0_DROP_BORADCAST, 0);
   rt2x00_set_field32(&reg, TXRX_CSR0_DROP_ACK_CTS, 1);
   rt2x00_register_write(rt2x00dev, TXRX_CSR0, reg);
   // Disable Synchronisation (TSF) because of our REAL-TIME Purposes.
   // TSF is used to keep the clocks of the STAs syncronized.
   // In Client mode the AP controls TSF, sent to STAs in beacons.
   // In ad-hoc (IBSS) mode the interval of the beacon (TBTT) is set up by the
   // STA that instances the IBSS network.
   // (more on this in rt61.h at TXRX_CSR9 register)
   rt2x00_register_read(rt2x00dev, TXRX_CSR9, &reg);
   // rt2x00_set_field32(&reg, TXRX_CSR9_BEACON_INTERVAL, 100 * 16);
   rt2x00_set_field32(&reg, TXRX_CSR9_TSF_TICKING, 0);
   rt2x00_set_field32(&reg, TXRX_CSR9_TBTT_ENABLE, 0);
   rt2x00_set_field32(&reg, TXRX_CSR9_TSF_SYNC_MODE, 1); // disable
   rt2x00_set_field32(&reg, TXRX_CSR9_BEACON_GEN, 0);
   rt2x00_register_write(rt2x00dev, TXRX_CSR9, reg);
}
// --------------------------------------------
//  7) INTERRUPT FUNCTIONS
// --------------------------------------------
static void rt61pci_rxdone(void *data)
{
   struct rt2x00_dev *rt2x00dev = &the_rt2x00_dev;
   struct data_ring *ring = data;
   struct data_entry *entry;
   void *rxd;
   u32 word0, word1;
   u16 size;
   unsigned int value;
	u8 reg_r17;

   while (1) {
      entry = rt2x00_get_data_entry(ring);
      rxd = rt2x00pci_desc_addr(entry);
      rt2x00_desc_read(rxd, 0, &word0);
      rt2x00_desc_read(rxd, 1, &word1);

      if (rt2x00_get_field32(word0, RXD_W0_OWNER_NIC))
         break;

      size = rt2x00_get_field32(word0, RXD_W0_DATABYTE_COUNT);

      if (!rt2x00_get_field32(word0, RXD_W0_CRC)) {

         rt2x00dev->rx_params.rate = device_signal_to_rate(
               &rt2x00dev->hw.modes[0],
               rt2x00_get_field32(word1, RXD_W1_SIGNAL),
               rt2x00_get_field32(word0, RXD_W0_OFDM));

         rt2x00dev->rx_params.ssi =
               rt2x00_get_field32(word1, RXD_W1_RSSI);

			rt2x00_bbp_read(rt2x00dev, 17, &reg_r17);
			rt2x00dev->rx_params.noise = reg_r17;

         DEBUG ("RATE: %d\n", rt2x00dev->rx_params.rate);
         DEBUG ("SSI: %d\n", rt2x00dev->rx_params.ssi);
			DEBUG ("NOISE: %d\n", rt2x00dev->rx_params.noise);

         insert_frame (&the_rx_ring_buffer,
                       (u8 *)rt2x00pci_data_addr(entry),
                       (u16) size,
                       rt2x00dev->rx_params.ssi);
         sem_getvalue (&irq_semaphore, &value);
         if (value == 0)
            sem_post (&irq_semaphore);
      }
      rt2x00_set_field32(&word0, RXD_W0_OWNER_NIC, 1);
      rt2x00_desc_write(rxd, 0, word0);
      rt2x00_ring_index_inc (ring);
   }
}
// -----------------------------------------------------------------------
static void rt61pci_txdone(void *data)
{
   struct data_ring *ring = data;
   struct rt2x00_dev *rt2x00dev = &the_rt2x00_dev;
   int index;
   int reg;

   DEBUG ("rt61pci_txdone\n");

   rt2x00_register_read(rt2x00dev, STA_CSR4, &reg);
   if (!rt2x00_get_field32(reg, STA_CSR4_VALID)) {
      printk ("STA_CSR4_VALID invalid\n");
      return;
   }
   ring = rt2x00_get_ring(rt2x00dev,
                          rt2x00_get_field32(reg, STA_CSR4_PID_TYPE));
   index = rt2x00_get_field32(reg, STA_CSR4_PID_SUBTYPE);
   DEBUG ("Index value of tx packet: %d\n", index);

   rt2x00_ring_index_done_inc(ring);
}
// -----------------------------------------------------------------------
static int rt61pci_interrupt(void* dev_instance, intr_t irq)
{
   struct rt2x00_dev *rt2x00dev = dev_instance;
   u32 reg;

   // Get the interrupt sources & saved to local variable.
   // Write register value back to clear pending interrupts.
   rt2x00_register_read(rt2x00dev, MCU_INT_SOURCE_CSR, &reg);
   rt2x00_register_write(rt2x00dev, MCU_INT_SOURCE_CSR, reg);

   rt2x00_register_read(rt2x00dev, INT_SOURCE_CSR, &reg);
   rt2x00_register_write(rt2x00dev, INT_SOURCE_CSR, reg);

   if (!reg) {
      DEBUG ("IRQ not for me (shared interrupt)\n");
      return POSIX_INTR_NOT_HANDLED;
   }

   if (!GET_FLAG(rt2x00dev, DEVICE_ENABLED_RADIO)) {
      DEBUG ("IRQ for me before initialization\n");
      return POSIX_INTR_HANDLED_NOTIFY;
   }

   // Handle interrupts, walk through all bits
   // and run the tasks, the bits are checked in order of
   // priority.

   // 1 - Beacon timer expired interrupt.
   if (rt2x00_get_field32(reg, INT_SOURCE_CSR_BEACON_DONE))
      DEBUG ("IRQ Beacon timer expired interrupt\n");
   // 2 - Rx ring done interrupt.
   if (rt2x00_get_field32(reg, INT_SOURCE_CSR_RXDONE)) {
      DEBUG ("IRQ Rx ring done interrupt\n");
      rt61pci_rxdone (&the_rt2x00_dev.ring[RING_RX]);
   }
   // 3 - Tx ring done interrupt.
   if (rt2x00_get_field32(reg, INT_SOURCE_CSR_TXDONE)) {
      DEBUG ("IRQ Tx ring done interrupt.\n");
      rt61pci_txdone (&the_rt2x00_dev.ring[RING_AC_VO]);
   }

   return POSIX_INTR_HANDLED_NOTIFY;
}
// --------------------------------------------
//  8) INITIALIZATION FUNCTIONS
// --------------------------------------------
static int rt61pci_init_firmware (struct rt2x00_dev *rt2x00dev)
{
   int i;
   u32 reg;
   u16 crc;
   struct firmware fw;
   fw.size = RT2561S_FIRMIM_LEN;
   fw.data = &RT2561S_FirmwareImage[0];

   // Wait for stable hardware.
   for (i = 0; i < 100; i++) {
      rt2x00_register_read(rt2x00dev, MAC_CSR0, &reg);
      if (reg)
         break;
      msleep(1);
   }

   if (!reg) {
      ERROR("Unstable hardware.\n");
      goto exit;
   }
   // Prepare MCU and mailbox for firmware loading.
   reg = 0;
   rt2x00_set_field32(&reg, MCU_CNTL_CSR_RESET, 1);
   rt2x00_register_write(rt2x00dev, MCU_CNTL_CSR, reg);
   rt2x00_register_write(rt2x00dev, M2H_CMD_DONE_CSR, 0xffffffff);
   rt2x00_register_write(rt2x00dev, H2M_MAILBOX_CSR, 0);
   rt2x00_register_write(rt2x00dev, HOST_CMD_CSR, 0);

   // Validate the firmware using 16 bit CRC.
   // The last 2 bytes of the firmware are the CRC
   // so substract those 2 bytes from the CRC checksum,
   // and set those 2 bytes to 0 when calculating CRC.
   reg = 0;
   crc = crc_itu_t(0, fw.data, fw.size - 2);
   crc = crc_itu_t(crc, (u8*)&reg, 2);

   if (crc != (fw.data[fw.size - 2] << 8 | fw.data[fw.size - 1])) {
      ERROR("Firmware CRC error.\n");
      goto exit;
   }
   DEBUG ("CRC Ok\n");

   rt2x00_set_chip_fw(&rt2x00dev->chip,
                       fw.data[fw.size - 4], fw.data[fw.size - 3]);

   // Write firmware to device.
   reg = 0;
   rt2x00_set_field32(&reg, MCU_CNTL_CSR_RESET, 1);
   rt2x00_set_field32(&reg, MCU_CNTL_CSR_SELECT_BANK, 1);
   rt2x00_register_write(rt2x00dev, MCU_CNTL_CSR, reg);

   rt2x00_register_multiwrite(
         rt2x00dev, FIRMWARE_IMAGE_BASE, (u32*)fw.data, fw.size);

   rt2x00_set_field32(&reg, MCU_CNTL_CSR_SELECT_BANK, 0);
   rt2x00_register_write(rt2x00dev, MCU_CNTL_CSR, reg);

   rt2x00_set_field32(&reg, MCU_CNTL_CSR_RESET, 0);
   rt2x00_register_write(rt2x00dev, MCU_CNTL_CSR, reg);

   for (i = 0; i < 100; i++) {
      rt2x00_register_read(rt2x00dev, MCU_CNTL_CSR, &reg);
      if (rt2x00_get_field32(reg, MCU_CNTL_CSR_READY))
         break;
      msleep(1);
   }

   if (i == 100) {
      ERROR("MCU Control register not ready.\n");
      goto exit;
   }

   // Reset MAC and BBP registers.
   reg = 0;
   rt2x00_set_field32(&reg, MAC_CSR1_SOFT_RESET, 1);
   rt2x00_set_field32(&reg, MAC_CSR1_BBP_RESET, 1);
   rt2x00_register_write(rt2x00dev, MAC_CSR1, reg);

   rt2x00_register_read(rt2x00dev, MAC_CSR1, &reg);
   rt2x00_set_field32(&reg, MAC_CSR1_SOFT_RESET, 0);
   rt2x00_set_field32(&reg, MAC_CSR1_BBP_RESET, 0);
   rt2x00_register_write(rt2x00dev, MAC_CSR1, reg);

   rt2x00_register_read(rt2x00dev, MAC_CSR1, &reg);
   rt2x00_set_field32(&reg, MAC_CSR1_HOST_READY, 1);
   rt2x00_register_write(rt2x00dev, MAC_CSR1, reg);

   SET_FLAG(rt2x00dev, FIRMWARE_LOADED);
   return 0;

exit:
      SET_FLAG(rt2x00dev, FIRMWARE_FAILED);
      ERROR("Firmware failed to load.\n");
      return -EIO;
}
// -----------------------------------------------------------------------
static int rt61pci_init_eeprom(struct rt2x00_dev *rt2x00dev)
{
   u32 reg;
   u16 value;
   u16 eeprom;
   u16 device;

   // 1 - Detect EEPROM width.
   rt2x00_register_read(rt2x00dev, E2PROM_CSR, &reg);
   if (rt2x00_get_field32(reg, E2PROM_CSR_TYPE_93C46))
      rt2x00dev->eeprom_width = EEPROM_WIDTH_93C46;
   else
      rt2x00dev->eeprom_width = EEPROM_WIDTH_93C66;

   // 2 - Read EEPROM word for configuration.
   rt2x00_eeprom_read(rt2x00dev, EEPROM_ANTENNA, &eeprom);

   // 3 - Identify RF chipset.
   // To determine the RT chip we have to read the
   // PCI header of the device.
   pci_read_config_word(rt2x00dev_pci(rt2x00dev),
                        PCI_CONFIG_HEADER_DEVICE, &device);
   value = rt2x00_get_field16(eeprom, EEPROM_ANTENNA_RF_TYPE);
   rt2x00_register_read(rt2x00dev, MAC_CSR0, &reg);
   rt2x00_set_chip(&rt2x00dev->chip, device, value, reg);

   if (!rt2x00_rf(&rt2x00dev->chip, RF5225) &&
        !rt2x00_rf(&rt2x00dev->chip, RF5325) &&
        !rt2x00_rf(&rt2x00dev->chip, RF2527) &&
        !rt2x00_rf(&rt2x00dev->chip, RF2529))
      return -ENODEV;

   DEBUG ("The CHIP is: %X (RF5325 0x0002 RF2527 0x0003 RF2529 0x0004)", rt2x00dev->chip.rf);

   // 4 - Identify default antenna configuration.
   // Ralink devices have have antenna options for both TX as RX.
   // The ieee80211 stack currently only provide the user to set
   // 1 antenna, by default this is considered to be the TX antenna.

   //    conf->antenna_sel = rt2x00_get_field16(eeprom,
   //                                           EEPROM_ANTENNA_TX_DEFAULT);

   // 5 - Read the Frame type.
   if (rt2x00_get_field16(eeprom, EEPROM_ANTENNA_FRAME_TYPE))
      SET_FLAG(rt2x00dev, CONFIG_FRAME_TYPE);

   // 6 - Determine number of antenna's.
   if (rt2x00_get_field16(eeprom, EEPROM_ANTENNA_NUM) == 2)
      SET_FLAG(rt2x00dev, CONFIG_DOUBLE_ANTENNA);

   // 7 - Read frequency offset and RF programming sequence.
   rt2x00_eeprom_read(rt2x00dev, EEPROM_FREQ, &eeprom);
   if (rt2x00_get_field16(eeprom, EEPROM_FREQ_SEQ_MASK) != 0xff &&
       rt2x00_get_field16(eeprom, EEPROM_FREQ_SEQ))
      SET_FLAG(rt2x00dev, CONFIG_RF_SEQUENCE);

   rt2x00dev->freq_offset = rt2x00_get_field16(eeprom,
                                               EEPROM_FREQ_OFFSET);
   if (rt2x00dev->freq_offset == 0xff)
      rt2x00dev->freq_offset = 0;

   // 8 - Read external LNA informations.
   rt2x00_eeprom_read(rt2x00dev, EEPROM_NIC, &eeprom);
   if (eeprom == 0xffff)
      eeprom = 0;
   if (rt2x00_get_field16(eeprom, EEPROM_NIC_EXTERNAL_LNA_A))
      SET_FLAG(rt2x00dev, CONFIG_EXTERNAL_LNA_A);
   if (rt2x00_get_field16(eeprom, EEPROM_NIC_EXTERNAL_LNA_BG))
      SET_FLAG(rt2x00dev, CONFIG_EXTERNAL_LNA_BG);

   // 9 - Store led settings, for correct led behaviour.
   rt2x00_eeprom_read(rt2x00dev, EEPROM_LED, &eeprom);

   // If the eeprom value is invalid,
   // switch to default led mode.
   if (eeprom == 0xffff)
      rt2x00dev->led_mode = LED_MODE_DEFAULT;
   else
      rt2x00dev->led_mode = rt2x00_get_field16(eeprom,
                                               EEPROM_LED_LED_MODE);

   rt2x00_set_field16(&rt2x00dev->led_reg, MCU_LEDCS_LED_MODE,
                       rt2x00dev->led_mode);
   rt2x00_set_field16(&rt2x00dev->led_reg, MCU_LEDCS_POLARITY_GPIO_0,
                       rt2x00_get_field16(eeprom, EEPROM_LED_POLARITY_GPIO_0));
   rt2x00_set_field16(&rt2x00dev->led_reg, MCU_LEDCS_POLARITY_GPIO_1,
                       rt2x00_get_field16(eeprom, EEPROM_LED_POLARITY_GPIO_1));
   rt2x00_set_field16(&rt2x00dev->led_reg, MCU_LEDCS_POLARITY_GPIO_2,
                       rt2x00_get_field16(eeprom, EEPROM_LED_POLARITY_GPIO_2));
   rt2x00_set_field16(&rt2x00dev->led_reg, MCU_LEDCS_POLARITY_GPIO_3,
                       rt2x00_get_field16(eeprom, EEPROM_LED_POLARITY_GPIO_3));
   rt2x00_set_field16(&rt2x00dev->led_reg, MCU_LEDCS_POLARITY_GPIO_4,
                       rt2x00_get_field16(eeprom, EEPROM_LED_POLARITY_GPIO_4));
   rt2x00_set_field16(&rt2x00dev->led_reg, MCU_LEDCS_POLARITY_ACT,
                       rt2x00_get_field16(eeprom, EEPROM_LED_POLARITY_ACT));
   rt2x00_set_field16(&rt2x00dev->led_reg, MCU_LEDCS_POLARITY_READY_BG,
                       rt2x00_get_field16(eeprom, EEPROM_LED_POLARITY_RDY_G));
   rt2x00_set_field16(&rt2x00dev->led_reg, MCU_LEDCS_POLARITY_READY_A,
                       rt2x00_get_field16(eeprom, EEPROM_LED_POLARITY_RDY_A));

   // 10 - Detect if this device has an hardware controlled radio.
   if (rt2x00_get_field16(eeprom, EEPROM_ANTENNA_HARDWARE_RADIO))
      SET_FLAG(rt2x00dev, DEVICE_SUPPORT_HW_BUTTON);

   // 11 - Read BBP data from EEPROM and store in private structure.
   rt2x00dev->eeprom = kzalloc(EEPROM_BBP_SIZE * sizeof(u16), GFP_KERNEL);
   if (!rt2x00dev->eeprom)
      return -ENOMEM;

   rt2x00_eeprom_multiread(rt2x00dev, EEPROM_BBP_START,
                           rt2x00dev->eeprom, EEPROM_BBP_SIZE * sizeof(u16));
   return 0;
}
// -----------------------------------------------------------------------
static int rt61pci_init_mac(struct rt2x00_dev *rt2x00dev)
{
   // Read MAC address from EEPROM.
   rt2x00_eeprom_multiread(rt2x00dev, EEPROM_MAC_ADDR_0,
                           (u16*)&the_mac_address[0], 6);
   // Check if a valid MAC address has been read. a
   if (!is_valid_ether_addr(&the_mac_address[0]))
      return -EINVAL;
   // Write MAC address to register.
   rt61pci_config_mac_address(rt2x00dev, &the_mac_address[0]);

   SET_FLAG(rt2x00dev, DEVICE_INITIALIZED_MAC);
   return 0;
}
// -----------------------------------------------------------------------
static int rt61pci_alloc_rings(struct rt2x00_dev *rt2x00dev)
{
   rt2x00dev->ring = kzalloc (sizeof(struct data_ring) * RING_NUM, GFP_KERNEL);
   if (!rt2x00dev->ring) {
      ERROR("Ring allocation failed.\n");
      return -ENOMEM;
   }
   return 0;
}
// -----------------------------------------------------------------------
static int rt61pci_alloc_dma_ring(struct rt2x00_dev *rt2x00dev,
	enum ring_index ring_type, const u16 max_entries,
        const u16 data_size, const u16 desc_size)
{
	struct data_ring *ring = &rt2x00dev->ring[ring_type];
	unsigned int i;

	ring->stats.limit = max_entries;
	ring->data_size = data_size;
	ring->desc_size = desc_size;

	// Allocate all ring entries.
	ring->entry = kmalloc(ring->stats.limit * sizeof(struct data_entry),
		              GFP_KERNEL);
	if (!ring->entry)
		return -ENOMEM;

	// Allocate DMA memory for descriptor and buffer.
	ring->data_addr = pci_alloc_consistent(rt2x00dev_pci(rt2x00dev),
		rt2x00_get_ring_size(ring), &ring->data_dma);
	if (!ring->data_addr) {
		kfree(ring->entry);
		return -ENOMEM;
	}

	// Initialize all ring entries to contain valid
	// addresses.
	for (i = 0; i < ring->stats.limit; i++) {
		ring->entry[i].ring = ring;
		ring->entry[i].priv = ring->data_addr
			+ (i * ring->desc_size);
		ring->entry[i].data_addr = ring->data_addr
			+ (ring->stats.limit * ring->desc_size)
			+ (i * ring->data_size);
		ring->entry[i].data_dma = ring->data_dma
			+ (ring->stats.limit * ring->desc_size)
			+ (i * ring->data_size);
	}

	return 0;
}
// -----------------------------------------------------------------------
static int rt61pci_allocate_dma_rings(struct rt2x00_dev *rt2x00dev)
{
   if (rt61pci_alloc_dma_ring(rt2x00dev, RING_RX,
                              RX_ENTRIES, DATA_FRAME_SIZE, RXD_DESC_SIZE) ||
       rt61pci_alloc_dma_ring(rt2x00dev, RING_AC_VO,
                              TX_ENTRIES, DATA_FRAME_SIZE, TXD_DESC_SIZE) ) {
      return -ENOMEM;
   }
   return 0;
}
// -----------------------------------------------------------------------
static void rt61pci_init_rxring(struct rt2x00_dev *rt2x00dev,
                                enum ring_index ring_type)
{
   struct data_ring *ring = &rt2x00dev->ring[ring_type];
   void *rxd;
   unsigned int i;
   u32 word;

   memset(ring->data_addr, 0x00, rt2x00_get_ring_size(ring));

   ring->type = ring_type;

   for (i = 0; i < ring->stats.limit; i++) {
      rxd = rt2x00pci_desc_addr(&ring->entry[i]);

      rt2x00_desc_read(rxd, 5, &word);
      rt2x00_set_field32(&word, RXD_W5_BUFFER_PHYSICAL_ADDRESS,
                          ring->entry[i].data_dma);
      rt2x00_desc_write(rxd, 5, word);

      rt2x00_desc_read(rxd, 0, &word);
      rt2x00_set_field32(&word, RXD_W0_OWNER_NIC, 1);
      rt2x00_desc_write(rxd, 0, word);
   }

   rt2x00_ring_index_clear(ring);
}
// -----------------------------------------------------------------------
static void rt61pci_init_txring(struct rt2x00_dev *rt2x00dev,
                                enum ring_index ring_type)
{
   struct data_ring *ring = &rt2x00dev->ring[ring_type];
   void *txd;
   unsigned int i;
   u32 word;

   memset(ring->data_addr, 0x00, rt2x00_get_ring_size(ring));

   ring->type = ring_type;

   for (i = 0; i < ring->stats.limit; i++) {
      txd = rt2x00pci_desc_addr(&ring->entry[i]);

      rt2x00_desc_read(txd, 1, &word);
      rt2x00_set_field32(&word, TXD_W1_BUFFER_COUNT, 1);
      rt2x00_desc_write(txd, 1, word);

      rt2x00_desc_read(txd, 5, &word);
      rt2x00_set_field32(&word, TXD_W5_PID_TYPE, ring_type);
      rt2x00_set_field32(&word, TXD_W5_PID_SUBTYPE, i);
      rt2x00_desc_write(txd, 5, word);

      rt2x00_desc_read(txd, 6, &word);
      rt2x00_set_field32(&word, TXD_W6_BUFFER_PHYSICAL_ADDRESS,
                          ring->entry[i].data_dma);
      rt2x00_desc_write(txd, 6, word);

      rt2x00_desc_read(txd, 0, &word);
      rt2x00_set_field32(&word, TXD_W0_VALID, 0);
      rt2x00_set_field32(&word, TXD_W0_OWNER_NIC, 0);
      rt2x00_desc_write(txd, 0, word);
   }

   rt2x00_ring_index_clear(ring);
}
// -----------------------------------------------------------------------
static int rt61pci_init_rings(struct rt2x00_dev *rt2x00dev)
{
   u32 reg;

   // Initialize rings.
   rt61pci_init_rxring(rt2x00dev, RING_RX);
   rt61pci_init_txring(rt2x00dev, RING_AC_VO);

   // Initialize TX RING registers.
   // a) Size of the TX RING
   reg = 0;
   rt2x00_set_field32(&reg, TX_RING_CSR0_AC3_RING_SIZE,
                      rt2x00dev->ring[RING_AC_VO].stats.limit);
   rt2x00_register_write(rt2x00dev, TX_RING_CSR0, reg);
   // b) Descriptor size of the TX RING
   reg = 0;
   rt2x00_set_field32(&reg, TX_RING_CSR1_TXD_SIZE,
                      rt2x00dev->ring[RING_AC_VO].desc_size / 4);
   rt2x00_register_write(rt2x00dev, TX_RING_CSR1, reg);
   // c) Bse address of the TX RING
   reg = 0;
   rt2x00_set_field32(&reg, AC3_BASE_CSR_RING_REGISTER,
                       rt2x00dev->ring[RING_AC_VO].data_dma);
   rt2x00_register_write(rt2x00dev, AC3_BASE_CSR, reg);

   // Initialize RX RING registers.
   // a) RX Ring size and descriptor size
   reg = 0;
   rt2x00_set_field32(&reg, RX_RING_CSR_RING_SIZE,
                       rt2x00dev->ring[RING_RX].stats.limit);
   rt2x00_set_field32(&reg, RX_RING_CSR_RXD_SIZE,
                       rt2x00dev->ring[RING_RX].desc_size / 4);
   rt2x00_set_field32(&reg, RX_RING_CSR_RXD_WRITEBACK_SIZE, 4);
   rt2x00_register_write(rt2x00dev, RX_RING_CSR, reg);
   // b) RX base address
   reg = 0;
   rt2x00_set_field32(&reg, RX_BASE_CSR_RING_REGISTER,
                       rt2x00dev->ring[RING_RX].data_dma);
   rt2x00_register_write(rt2x00dev, RX_BASE_CSR, reg);

   rt2x00_register_write(rt2x00dev, TX_DMA_DST_CSR, 0x000000aa);
   rt2x00_register_write(rt2x00dev, LOAD_TX_RING_CSR, 0x0000001f);
   rt2x00_register_write(rt2x00dev, RX_CNTL_CSR, 0x00000002);

   return 0;
}
// -----------------------------------------------------------------------
static int rt61pci_init_registers(struct rt2x00_dev *rt2x00dev)
{
   u32 reg;

   rt2x00_register_write(rt2x00dev, MAC_CSR10, 0x00000718);

   rt2x00_register_write(rt2x00dev, TXRX_CSR0, 0x025eb032);

   rt2x00_register_write(rt2x00dev, TXRX_CSR1, 0x9eb39eb3);
   rt2x00_register_write(rt2x00dev, TXRX_CSR2, 0x8a8b8c8d);
   rt2x00_register_write(rt2x00dev, TXRX_CSR3, 0x00858687);

   rt2x00_register_write(rt2x00dev, TXRX_CSR7, 0x2e31353b);
   rt2x00_register_write(rt2x00dev, TXRX_CSR8, 0x2a2a2a2c);

   rt2x00_register_write(rt2x00dev, TXRX_CSR15, 0x0000000f);

   rt2x00_register_write(rt2x00dev, MAC_CSR6, 0x00000fff);

   rt2x00_register_write(rt2x00dev, MAC_CSR13, 0x0000e000);

   rt2x00_register_write(rt2x00dev, SEC_CSR0, 0x00000000);
   rt2x00_register_write(rt2x00dev, SEC_CSR1, 0x00000000);
   rt2x00_register_write(rt2x00dev, SEC_CSR5, 0x00000000);

   rt2x00_register_read(rt2x00dev, AC_TXOP_CSR0, &reg);
   rt2x00_set_field32(&reg, AC_TXOP_CSR0_AC0_TX_OP, 0);
   rt2x00_set_field32(&reg, AC_TXOP_CSR0_AC1_TX_OP, 0);
   rt2x00_register_write(rt2x00dev, AC_TXOP_CSR0, reg);

   rt2x00_register_read(rt2x00dev, AC_TXOP_CSR1, &reg);
   rt2x00_set_field32(&reg, AC_TXOP_CSR1_AC2_TX_OP, 192);
   rt2x00_set_field32(&reg, AC_TXOP_CSR1_AC3_TX_OP, 48);
   rt2x00_register_write(rt2x00dev, AC_TXOP_CSR1, reg);

   rt2x00_register_read(rt2x00dev, MAC_CSR9, &reg);
   rt2x00_set_field32(&reg, MAC_CSR9_CW_SELECT, 0);
   rt2x00_register_write(rt2x00dev, MAC_CSR9, reg);

   rt2x00_register_read(rt2x00dev, TXRX_CSR0, &reg);
   rt2x00_set_field32(&reg, TXRX_CSR0_AUTO_TX_SEQ, 1);
   rt2x00_register_write(rt2x00dev, TXRX_CSR0, reg);

   rt2x00_register_write(rt2x00dev, PHY_CSR1, 0x000023b0);
   rt2x00_register_write(rt2x00dev, PHY_CSR5, 0x060a100c);
   rt2x00_register_write(rt2x00dev, PHY_CSR6, 0x00080606);
   rt2x00_register_write(rt2x00dev, PHY_CSR7, 0x00000a08);

   rt2x00_register_write(rt2x00dev, PCI_CFG_CSR, 0x28ca4404);

   rt2x00_register_write(rt2x00dev, M2H_CMD_DONE_CSR, 0xffffffff);

   // We must clear the error counters.
   // These registers are cleared on read,
   // so we may pass a useless variable to store the value.
   rt2x00_register_read(rt2x00dev, STA_CSR0, &reg);
   rt2x00_register_read(rt2x00dev, STA_CSR1, &reg);
   rt2x00_register_read(rt2x00dev, STA_CSR2, &reg);

   // Reset MAC and BBP registers.
   reg = 0;
   rt2x00_set_field32(&reg, MAC_CSR1_SOFT_RESET, 1);
   rt2x00_set_field32(&reg, MAC_CSR1_BBP_RESET, 1);
   rt2x00_register_write(rt2x00dev, MAC_CSR1, reg);

   rt2x00_register_read(rt2x00dev, MAC_CSR1, &reg);
   rt2x00_set_field32(&reg, MAC_CSR1_SOFT_RESET, 0);
   rt2x00_set_field32(&reg, MAC_CSR1_BBP_RESET, 0);
   rt2x00_register_write(rt2x00dev, MAC_CSR1, reg);

   rt2x00_register_read(rt2x00dev, MAC_CSR1, &reg);
   rt2x00_set_field32(&reg, MAC_CSR1_HOST_READY, 1);
   rt2x00_register_write(rt2x00dev, MAC_CSR1, reg);

   return 0;
}
// -----------------------------------------------------------------------
static int rt61pci_init_bbp(struct rt2x00_dev *rt2x00dev)
{
   u8 reg_id;
   u8 value;
   unsigned int i;

   for (i = 0; i < REGISTER_BUSY_COUNT; i++) {
      rt2x00_bbp_read(rt2x00dev, 0, &value);
      if ((value != 0xff) && (value != 0x00))
         goto continue_csr_init;
      NOTICE("Waiting for BBP register.\n");
   }

   ERROR("BBP register access failed, aborting.\n");
   return -EACCES;

continue_csr_init:
      rt2x00_bbp_write(rt2x00dev, 3, 0x00);
	rt2x00_bbp_write(rt2x00dev, 15, 0x30);
        rt2x00_bbp_write(rt2x00dev, 17, 0x20);
        rt2x00_bbp_write(rt2x00dev, 21, 0xc8);
        rt2x00_bbp_write(rt2x00dev, 22, 0x38);
        rt2x00_bbp_write(rt2x00dev, 23, 0x06);
        rt2x00_bbp_write(rt2x00dev, 24, 0xfe);
        rt2x00_bbp_write(rt2x00dev, 25, 0x0a);
        rt2x00_bbp_write(rt2x00dev, 26, 0x0d);
        rt2x00_bbp_write(rt2x00dev, 34, 0x12);
        rt2x00_bbp_write(rt2x00dev, 37, 0x07);
        rt2x00_bbp_write(rt2x00dev, 39, 0xf8);
        rt2x00_bbp_write(rt2x00dev, 41, 0x60);
        rt2x00_bbp_write(rt2x00dev, 53, 0x10);
        rt2x00_bbp_write(rt2x00dev, 54, 0x18);
        rt2x00_bbp_write(rt2x00dev, 60, 0x10);
        rt2x00_bbp_write(rt2x00dev, 61, 0x04);
        rt2x00_bbp_write(rt2x00dev, 62, 0x04);
        rt2x00_bbp_write(rt2x00dev, 75, 0xfe);
        rt2x00_bbp_write(rt2x00dev, 86, 0xfe);
        rt2x00_bbp_write(rt2x00dev, 88, 0xfe);
        rt2x00_bbp_write(rt2x00dev, 90, 0x0f);
        rt2x00_bbp_write(rt2x00dev, 99, 0x00);
        rt2x00_bbp_write(rt2x00dev, 102, 0x16);
        rt2x00_bbp_write(rt2x00dev, 107, 0x04);

        DEBUG("Start initialization from EEPROM...\n");
        for (i = 0; i < EEPROM_BBP_SIZE; i++) {
           if (rt2x00dev->eeprom[i] != 0xffff &&
               rt2x00dev->eeprom[i] != 0x0000) {
              reg_id = rt2x00_get_field16(
                    rt2x00dev->eeprom[i], EEPROM_BBP_REG_ID);
              value = rt2x00_get_field16(
                    rt2x00dev->eeprom[i], EEPROM_BBP_VALUE);
              DEBUG("BBP: 0x%02x, value: 0x%02x.\n", reg_id, value);
              rt2x00_bbp_write(rt2x00dev, reg_id, value);
               }
        }
        DEBUG("...End initialization from EEPROM.\n");

        return 0;
}
// -----------------------------------------------------------------------
static int rt61pci_enable_radio(struct rt2x00_dev *rt2x00dev)
{
   u32 reg;

   // Don't enable the radio twice.
   if (GET_FLAG(rt2x00dev, DEVICE_ENABLED_RADIO)) {
      printk ("radio already initialized\n");
      return 0;
   }

   // Initialize all registers.
   if (rt61pci_init_rings(rt2x00dev) ||
       rt61pci_init_registers(rt2x00dev) ||
       rt61pci_init_bbp(rt2x00dev)) {
      ERROR("Register initialization failed.\n");
      goto exit_fail;
   }

   // Clear interrupts.
   rt2x00_register_read(rt2x00dev, INT_SOURCE_CSR, &reg);
   rt2x00_register_write(rt2x00dev, INT_SOURCE_CSR, reg);

   rt2x00_register_read(rt2x00dev, MCU_INT_SOURCE_CSR, &reg);
   rt2x00_register_write(rt2x00dev, MCU_INT_SOURCE_CSR, reg);

   // Enable interrupts.
   reg = 0;
   rt2x00_set_field32(&reg, INT_MASK_CSR_TX_ABORT_DONE, 1);
   rt2x00_set_field32(&reg, INT_MASK_CSR_MITIGATION_PERIOD, 0xff);
   rt2x00_register_write(rt2x00dev, INT_MASK_CSR, reg);

   rt2x00_register_write(rt2x00dev, MCU_INT_MASK_CSR, 0x00000000);

   // Enable RX.
   rt2x00_register_write(rt2x00dev, RX_CNTL_CSR, 0x00000001);
   rt2x00_register_read(rt2x00dev, TXRX_CSR0, &reg);
   rt2x00_set_field32(&reg, TXRX_CSR0_DISABLE_RX, 0);
   rt2x00_register_write(rt2x00dev, TXRX_CSR0, reg);

   // Enable LED
   rt61pci_enable_led(rt2x00dev);

   SET_FLAG(rt2x00dev, DEVICE_ENABLED_RADIO);

   DEBUG("Radio Enabled\n");
   return 0;

exit_fail:
      //rt61pci_uninitialize(rt2x00dev);
   return -EIO;
}
// -----------------------------------------------------------------------
static void rt61pci_init_hw_channels(struct rt2x00_dev *rt2x00dev,
                                     struct ieee80211_channel *channels)
{
   unsigned int i;
   u16 eeprom;

   // Channel initialization.
   // First we set the basic variables.
   for (i = 0; i < 13; i++) {
      channels[i].chan = i + 1;
      channels[i].freq = 2407 + ((i + 1) * 5);
      channels[i].flag = IEEE80211_CHAN_W_IBSS |
            IEEE80211_CHAN_W_ACTIVE_SCAN | IEEE80211_CHAN_W_SCAN;
      channels[i].antenna_max = 0xff;
   }

   channels[13].chan = 14;
   channels[13].freq = 2484;
   channels[13].flag = IEEE80211_CHAN_W_IBSS |
         IEEE80211_CHAN_W_ACTIVE_SCAN | IEEE80211_CHAN_W_SCAN;
   channels[13].antenna_max = 0xff;

   // Set device specific value.
   if (!GET_FLAG(rt2x00dev, CONFIG_RF_SEQUENCE)) {
      static const u32 vals[] = {
         0x00004786, 0x00004786, 0x0000478a, 0x0000478a,
         0x0000478e, 0x0000478e, 0x00004792, 0x00004792,
         0x00004796, 0x00004796, 0x0000479a, 0x0000479a,
         0x0000479e, 0x000047a2
      };

      for (i = 0; i < ARRAY_SIZE(vals); i++)
         channels[i].val = vals[i];
   } else {
      static const u32 vals[] = {
         0x00004786, 0x00004786, 0x0000478a, 0x0000478a,
         0x0000478e, 0x0000478e, 0x00004792, 0x00004792,
         0x00004796, 0x00004796, 0x0000479a, 0x0000479a,
         0x0000479e, 0x000047a2
      };

      for (i = 0; i < ARRAY_SIZE(vals); i++)
         channels[i].val = vals[i];
   }

   // Set TX power, each EEPROM TXpower entry
   // contains the TXpower value for 2 channels.
   for (i = 0; i < EEPROM_TXPOWER_G_SIZE; i++) {
      rt2x00_eeprom_read(rt2x00dev,
                        EEPROM_TXPOWER_G_START + i, &eeprom);

      channels[(i * 2)].power_level = TXPOWER_FROM_DEV(
            rt2x00_get_field16(eeprom, EEPROM_TXPOWER_G_1));

      channels[(i * 2) + 1].power_level = TXPOWER_FROM_DEV(
            rt2x00_get_field16(eeprom, EEPROM_TXPOWER_G_2));
   }
}
// -----------------------------------------------------------------------
static void rt61pci_init_hw_rates(struct rt2x00_dev *rt2x00dev,
                                  struct ieee80211_rate *rates)
{
   // Rates initialization.
   device_rate_entry(&rates[0], 10, 0x001, 0x00, IEEE80211_RATE_CCK);
   device_rate_entry(&rates[1], 20, 0x003, 0x01, IEEE80211_RATE_CCK_2);
   device_rate_entry(&rates[2], 55, 0x007, 0x02, IEEE80211_RATE_CCK_2);
   device_rate_entry(&rates[3], 110, 0x00f, 0x03, IEEE80211_RATE_CCK_2);
   device_rate_entry(&rates[4], 60, 0x01f, 0x0b, IEEE80211_RATE_OFDM);
   device_rate_entry(&rates[5], 90, 0x03f, 0x0f, IEEE80211_RATE_OFDM);
   device_rate_entry(&rates[6], 120, 0x07f, 0x0a, IEEE80211_RATE_OFDM);
   device_rate_entry(&rates[7], 180, 0x0ff, 0x0e, IEEE80211_RATE_OFDM);
   device_rate_entry(&rates[8], 240, 0x1ff, 0x09, IEEE80211_RATE_OFDM);
   device_rate_entry(&rates[9], 360, 0x3ff, 0x0d, IEEE80211_RATE_OFDM);
   device_rate_entry(&rates[10], 480, 0x7ff, 0x08, IEEE80211_RATE_OFDM);
   device_rate_entry(&rates[11], 540, 0xfff, 0x0c, IEEE80211_RATE_OFDM);
}
// -----------------------------------------------------------------------
static int rt61pci_init_hw_modes(struct rt2x00_dev *rt2x00dev)
{
   struct ieee80211_hw *hw = &rt2x00dev->hw;
   int num_modes;
   int num_channels;

   // RF2527 and RF2529 only supports 802.11b & 802.11g,
   // so we should allocate 14 OFDM channels, 4 CCK rates
   // and 8 OFDM rates.
   num_modes = 2;
   num_channels = 14;

   hw->num_modes = num_modes;
   hw->modes =
         kzalloc((sizeof(struct ieee80211_hw_modes) * num_modes),
                  GFP_KERNEL);
   if (!hw->modes)
      goto exit;

   hw->modes->channels =
         kzalloc((sizeof(struct ieee80211_channel) * num_channels),
                  GFP_KERNEL);
   if (!hw->modes->channels)
      goto exit_free_modes;

   hw->modes->rates =
         kzalloc((sizeof(struct ieee80211_rate) * 12),
                  GFP_KERNEL);
   if (!hw->modes->rates)
      goto exit_free_channels;

   // Intitialize 802.11g
   // Rates: CCK, OFDM.
   // Channels: OFDM.
   hw->modes[0].mode = MODE_IEEE80211G;
   hw->modes[0].num_channels = 14;
   hw->modes[0].num_rates = 12;

   // Intitialize 802.11b
   // Rates: CCK.
   // Channels: OFDM.
   hw->modes[1].mode = MODE_IEEE80211B;
   hw->modes[1].num_channels = 14;
   hw->modes[1].num_rates = 4;
   hw->modes[1].channels = hw->modes[0].channels;
   hw->modes[1].rates = hw->modes[0].rates;

   rt61pci_init_hw_channels(rt2x00dev, hw->modes[0].channels);
   rt61pci_init_hw_rates(rt2x00dev, hw->modes[0].rates);

   return 0;

exit_free_channels:
      kfree(hw->modes->channels);
	hw->modes->channels = NULL;

exit_free_modes:
      kfree(hw->modes);
	hw->modes = NULL;

exit:
      ERROR("Allocation ieee80211 modes failed.\n");
	return -ENOMEM;
}
//------------------------------------------------------------------
static int rt61pci_frame_init (wifi_frame_t *frame) {
   struct ieee80211_hdr *header =  (struct ieee80211_hdr *) frame->info;

   if (header == NULL)
      return -1;
   header->frame_control = 0x4000;
   header->duration_id = 0x0000;
   memcpy (header->addr2, the_mac_address, 6);
   header->seq_ctrl = 0x0000;
   frame->len = 0;
   return 0;
}
// --------------------------------------------
//  9) TX FUNCTIONS
// --------------------------------------------
static void rt61pci_write_tx_desc(struct rt2x00_dev *rt2x00dev,
                                  void *txd, const wifi_frame_t *frame)
{
   struct data_ring *ring;
   int tx_rate;
   u32 word;
   u32 length;
   u32 residual;
   u16 length_high;
   u16 length_low;
   char ofdm_rate;
   char req_timestamp;
   char more_frag;
   char req_seq;
   char ifs;
   char queue;
   u8 signal;
   u8 service;
   u8 bitrate;

   DEBUG ("rt61pci_write_tx_desc\n");

   // We require the ring structure this packet is being send to.
   ring = rt2x00_get_ring(rt2x00dev, RING_AC_VO);
   if (unlikely(!ring))
      return;

   // Check which rate should be used for this frame.
   tx_rate = 4106;
   // Are we working with OFDM rates.
   ofdm_rate = 0;
   // Check if more fragments will follow this frame.
   more_frag = 0;
   // Check if we require to enable the hw sequence counter.
   req_seq = 0;
   // Beacons and probe responses require the tsf timestamp
   // to be inserted into the frame.
   req_timestamp = 0;
   // Determine with what IFS priority this frame should be send.
   // Set ifs to IFS_SIFS when the this is not the first fragment,
   // or this fragment came after RTS/CTS.
   ifs = 0;
   // Determine queue identification number
   queue = 0;
   // Add 4 bytes for FCS.
   length = frame->len + FCS_LEN;
   // How the length should be processed depends
   // on if we are working with OFDM rates or not.
   if (ofdm_rate) {
      residual = 0;
      length_high = (length >> 6) & 0x3f;
      length_low = (length & 0x3f);
   } else {
      bitrate = DEVICE_GET_RATE_FIELD(tx_rate, RATE);
      // printk ("bitrate %u (10?)\n",bitrate);
      // Convert length to microseconds.
      residual = get_duration_res(length, bitrate);
      length = get_duration(length, bitrate);

      if (residual != 0)
         length++;

      length_high = length >> 8;
      length_low = length & 0xff;
   }

   // Create the signal and service values.
   signal = 0;
   service = 0x04;
   if (residual <= (8 % 11))
      service |= 0x80;

   // Start writing the descriptor words.
   rt2x00_desc_read(txd, 1, &word);
   rt2x00_set_field32(&word, TXD_W1_HOST_Q_ID, queue);
   rt2x00_set_field32(&word, TXD_W1_AIFSN, 2);
   rt2x00_set_field32(&word, TXD_W1_CWMIN, 4);
   rt2x00_set_field32(&word, TXD_W1_CWMAX, 10);
   rt2x00_set_field32(&word, TXD_W1_IV_OFFSET, IEEE80211_HEADER);
   rt2x00_set_field32(&word, TXD_W1_HW_SEQUENCE, req_seq);
   rt2x00_desc_write(txd, 1, word);

   rt2x00_desc_read(txd, 2, &word);
   rt2x00_set_field32(&word, TXD_W2_PLCP_SIGNAL, signal);
   rt2x00_set_field32(&word, TXD_W2_PLCP_SERVICE, service);
   rt2x00_set_field32(&word, TXD_W2_PLCP_LENGTH_LOW, length_low);
   rt2x00_set_field32(&word, TXD_W2_PLCP_LENGTH_HIGH, length_high);
   rt2x00_desc_write(txd, 2, word);

   rt2x00_desc_read(txd, 5, &word);
   rt2x00_set_field32(&word, TXD_W5_TX_POWER,
                       TXPOWER_TO_DEV(27));
   rt2x00_set_field32(&word, TXD_W5_WAITING_DMA_DONE_INT, 1);
   rt2x00_desc_write(txd, 5, word);

   rt2x00_desc_read(txd, 11, &word);
   rt2x00_set_field32(&word, TXD_W11_BUFFER_LENGTH0, frame->len);
   rt2x00_desc_write(txd, 11, word);

   rt2x00_desc_read(txd, 0, &word);
   rt2x00_set_field32(&word, TXD_W0_OWNER_NIC, 1);
   rt2x00_set_field32(&word, TXD_W0_VALID, 1);
   rt2x00_set_field32(&word, TXD_W0_MORE_FRAG, more_frag);
   rt2x00_set_field32(&word, TXD_W0_ACK, 0);
   rt2x00_set_field32(&word, TXD_W0_TIMESTAMP, req_timestamp);
   rt2x00_set_field32(&word, TXD_W0_OFDM, ofdm_rate);
   rt2x00_set_field32(&word, TXD_W0_IFS, ifs);
   rt2x00_set_field32(&word, TXD_W0_RETRY_MODE, 0);
   rt2x00_set_field32(&word, TXD_W0_TKIP_MIC, 0);
   rt2x00_set_field32(&word, TXD_W0_DATABYTE_COUNT, frame->len);
   rt2x00_set_field32(&word, TXD_W0_CIPHER_ALG, CIPHER_NONE);
   rt2x00_desc_write(txd, 0, word);
}
//------------------------------------------------------------------
static int rt61pci_tx (const wifi_frame_t *frame)
{
   struct rt2x00_dev *rt2x00dev = &the_rt2x00_dev;
   struct data_ring *ring;
   struct data_entry *entry;
   void *txd;
   u32 reg;

#ifdef RT61_ENABLE_TIME_MEASUREMENTS
   time_measure_posix_begin(rt61_measurement_id);
#endif

   // Determine which ring to put packet on.
   ring = rt2x00_get_ring (rt2x00dev, RING_AC_VO);

   if (!ring) {
      ERROR("rt61pci_tx: bad ring\n");
      return -1;
   }

   if (rt2x00_ring_full(ring)) {
      ERROR("rt61pci_tx: rt2x00_ring_full\n");
      return -1;
   }
   entry = rt2x00_get_data_entry(ring);
   txd = rt2x00pci_desc_addr(entry);
   rt2x00_desc_read(txd, 0, &reg);

   memcpy(rt2x00pci_data_addr(entry), frame->info, frame->len);
   rt61pci_write_tx_desc(rt2x00dev, txd, frame);

   rt2x00_ring_index_inc(ring);

   if (rt2x00_ring_full(ring))
      ERROR("rt61pci_tx: full ring\n");

   rt2x00_register_read(rt2x00dev, TX_CNTL_CSR, &reg);
   rt2x00_set_field32(&reg, TX_CNTL_CSR_KICK_TX_AC3, 1);
   rt2x00_register_write(rt2x00dev, TX_CNTL_CSR, reg);

#ifdef RT61_ENABLE_TIME_MEASUREMENTS
   time_measure_posix_end(rt61_measurement_id, NULL);
#endif

   return 0;
}
// --------------------------------------------
//  10) APPLICATION INTERFACE FUNCTIONS
// --------------------------------------------
int rt61_init (int channel, int txpower)
{
   static struct pci_device dev, *dev_p;
   static struct rt2x00_dev *rt2x00dev = &the_rt2x00_dev;
   //-------------------------
   //  a) PCI INITIALIZATION
   //-------------------------
   // Find the card in the PCI bus
   dev_p = &dev;
   if (pci_find_device(RT2561_VENDOR, RT2561_DEVICE, NULL, dev_p)) {
      ERROR ("Device not found\n");
      return -ENODEV;
   }
   // Set it as bus master and adjust latency
   adjust_pci_device(dev_p);
   DEBUG("Device Adjusted!\n");
   // Get the base address for our registers
   rt2x00dev->dev = dev_p;
   rt2x00dev->csr_addr = (void *)pci_bar_start (dev_p, PCI_BASE_ADDRESS_0);
   DEBUG ("Device Base Address: %X\n", (u32)rt2x00dev->csr_addr);
   //------------------
   //  b) READ EEPROM
   //------------------
   // Read important data (MAC address, chipset, antenna, led info, ..) from the EEPROM
   if (rt61pci_init_eeprom(rt2x00dev) || rt61pci_init_mac(rt2x00dev)) {
      ERROR("Failed to initialize device.\n");
      return -ENODEV;
   }
   //--------------------
   //  c) LOAD FIRMWARE
   //--------------------
   // Load the Firmware image to the MCU 8051 controller
   if (rt61pci_init_firmware(rt2x00dev)) {
      ERROR("Failed to load Firmware.\n");
      return -ENODEV;
   }
   //-------------------------------
   //  d) INIT SOFTWARE STRUCTURES
   //-------------------------------
   // Calculate the 802.11 values (rate, channels...) and save them to the_rt2x00_dev.hw
   if (rt61pci_init_hw_modes(rt2x00dev)) {
      ERROR("Failed to allocate rings.\n");
      return -ENOMEM;
   }
   // Allocate memory for RING_NUM rings (just a malloc)
   if (rt61pci_alloc_rings(rt2x00dev)) {
      ERROR("Failed to allocate rings.\n");
      return -ENOMEM;
   }
   // Allocate all data rings.
   if (rt61pci_allocate_dma_rings(rt2x00dev)) {
      ERROR("DMA allocation failed.\n");
      goto exit_fail;
   }
   // Init the circular rx buffer for storing received frames
   init_ring_buffer (&the_rx_ring_buffer);
   // Init the tx frame fields
   rt61pci_frame_init (&the_tx_frame);
   //---------------------------
   //  e) REGISTER IRQ HANDLER
   //---------------------------
   // Initalize the semaphore used to synchronize threads with the irq handler
   if (sem_init (&irq_semaphore, 0, 0)) {
      ERROR("Failed to initialize the irq_semaphore.\n");
      goto exit_fail;
   }
   // Associate and Unlock the irq handler
   if (posix_intr_associate (rt2x00dev_pci(rt2x00dev)->irq, rt61pci_interrupt,
       rt2x00dev, sizeof(struct rt2x00_dev) ) ||
       posix_intr_unlock (rt2x00dev_pci(rt2x00dev)->irq) ) {
      ERROR ("IRQ %d allocation failed.\n", rt2x00dev_pci(rt2x00dev)->irq);
      goto exit_fail;
   }
   //---------------------------
   //  f) ENABLE RADIO!
   //---------------------------
   if (rt61pci_enable_radio(rt2x00dev)) {
      ERROR("rt61pci_enable_radio\n");
      goto exit_fail;
   }
   //------------------------------------------------
   //  f) CONFIG CHANNEL, TXPOWER,  FILTERs and SYNC
   //------------------------------------------------
   rt61pci_config_channel (rt2x00dev,
                           rt2x00dev->hw.modes[0].channels[channel - 1].val,
                           rt2x00dev->hw.modes[0].channels[channel - 1].chan,
                           rt2x00dev->hw.modes[0].channels[channel - 1].freq,
                           txpower);
   rt61pci_config_filter_sync (rt2x00dev);

#ifdef RT61_ENABLE_TIME_MEASUREMENTS
    int err;
    err = time_measure_posix_create("rt61_measurement",
                                    CLOCK_THREAD_CPUTIME_ID,
                                    &rt61_measurement_id);
    assert(err == 0);
#endif

   return 0;

exit_fail:
   // rt61pci_free_rings(rt2x00dev);
   return -EIO;
}
//------------------------------------------------------------------
void rt61pci_enable_promisc (void)
{
   rt61pci_config_promisc(&the_rt2x00_dev, 1);
}
//------------------------------------------------------------------
void rt61pci_disable_promisc (void)
{
   rt61pci_config_promisc(&the_rt2x00_dev, 0);
}
//------------------------------------------------------------------
int rt61pci_recv (wifi_frame_t *frame, const struct timespec *abs_timeout) {
   struct rt2x00_dev *rt2x00dev = &the_rt2x00_dev;
   struct data_ring *ring;
   int value;

   ring = rt2x00_get_ring(rt2x00dev, RING_RX);

   // Wait for data to be available
   if (abs_timeout == NULL) {
      if (sem_wait (&irq_semaphore)) {
         ERROR("sem_wait failed\n");
         return -1;
      }
   } else {
      if (sem_timedwait (&irq_semaphore, abs_timeout)) {
         ERROR("sem_timedwait timeout exceeded\n");
         return -1;
      }
   }

   if (posix_intr_lock (rt2x00dev_pci(rt2x00dev)->irq)) {
      ERROR("posix_intr_lock failed\n");
      return -1;
   }
   //----------------------------------------------
   extract_frame (&the_rx_ring_buffer, frame);
   if (!ring_buffer_empty (&the_rx_ring_buffer)) {
      sem_getvalue (&irq_semaphore, &value);
      if (value == 0)
         sem_post (&irq_semaphore);
   }
   //----------------------------------------------
   if (posix_intr_unlock (rt2x00dev_pci(rt2x00dev)->irq)) {
      ERROR("posix_intr_lock failed\n");
      return -1;
   }

   return 0;
}
//------------------------------------------------------------------
static int rt61pci_frame_setDestMac (wifi_frame_t *frame, const mac_address addr) {
   struct ieee80211_hdr *header =  (struct ieee80211_hdr *) frame->info;

   if (header == NULL)
      return -1;
   memcpy (header->addr1, addr, 6);
   memcpy (header->addr3, addr, 6);
   return 0;
}
//------------------------------------------------------------------
static int rt61pci_frame_setData (wifi_frame_t *frame, const unsigned char *buff, const unsigned short nbytes) {
   if (frame == NULL)
      return -1;
   if (nbytes > DATA_FRAME_SIZE - HEADER_SIZE) {
      ERROR ("nbytes is too big\n");
      return -1;
   }
   memcpy (&frame->info [HEADER_SIZE], buff, nbytes);
   frame->len = nbytes + HEADER_SIZE;
   return 0;
}
//------------------------------------------------------------------
int rt61pci_send (const unsigned char *buff, const int nbytes, const mac_address to) {
   if (rt61pci_frame_setDestMac (&the_tx_frame, to) ||
       rt61pci_frame_setData (&the_tx_frame, buff, nbytes)) {
      ERROR ("could not set tx frame fields\n");
      return -1;
   }
   return rt61pci_tx (&the_tx_frame);
}
//------------------------------------------------------------------
int rt61pci_frame_getSourceMac (const wifi_frame_t *frame, mac_address addr) {
   struct ieee80211_hdr *header =  (struct ieee80211_hdr *) frame->info;

   if (header == NULL)
      return -1;
   memcpy (addr, header->addr2, 6);
   return 0;
}
//------------------------------------------------------------------
int rt61pci_frame_getData (const wifi_frame_t *frame, unsigned char *buff, const unsigned short nbytes) {
   unsigned short read_bytes;

   if (frame == NULL)
      return -1;

   if (frame->len < 24) {
      ERROR ("frame->len too small\n");
      return -1;
   }

   read_bytes = nbytes;
   if (read_bytes > frame->len - 24) {
      read_bytes = frame->len - 24;
   }
   memcpy (buff, &frame->info [24], read_bytes);
   return (int) read_bytes;
}
//------------------------------------------------------------------
void rt61pci_led_on (void) {
   rt61pci_enable_led(&the_rt2x00_dev);
}
//------------------------------------------------------------------
void rt61pci_led_off (void) {
   rt61pci_disable_led(&the_rt2x00_dev);
}
//------------------------------------------------------------------
void rt61pci_config_channel_txpower (int channel, int txpower) {
   struct rt2x00_dev *rt2x00dev = &the_rt2x00_dev;

   rt61pci_config_channel (rt2x00dev,
                           rt2x00dev->hw.modes[0].channels[channel - 1].val,
                           rt2x00dev->hw.modes[0].channels[channel - 1].chan,
                           rt2x00dev->hw.modes[0].channels[channel - 1].freq,
                           txpower);
}
//------------------------------------------------------------------
int rt61pci_radio_on (void) {
   return rt61pci_enable_radio(&the_rt2x00_dev);
}


