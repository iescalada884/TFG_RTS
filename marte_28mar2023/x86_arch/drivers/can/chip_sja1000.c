/*!
 * @file chip_sja1000.c
 *
 * @brief module for the chip sja1000
 *
 * @version 0.01
 *
 * @date 11-Feb-2008
 *
 * @author
 *      Juan Lopez (original author)
 *      Daniel Sangorrin (rewriting, documentation and reorganization)
 *
 * @comments
 *
 * This module contains the code that manages sja1000 chips.
 *
 * @license
 *
 * See MaRTE OS license
 *
 */

#include "pio.h"    // inb, outb
#include <stdint.h> // uint32_t, uint16_t, uint8_t
#include <stdbool.h>
#include <misc/linux_list.h> // list_xxxx

#include <drivers/can.h>
#include "can_debug.h"
#include "chip_sja1000.h"

#define MX_RETRIES 1000

/**
 * sja1000_outb
 *
 * macros for writing a byte into a sja1000 register
 *
 **/

static inline void sja1000_outb(const struct can_chip_t *chip,
                                uint8_t data,
                                enum sja1000_PeliCAN_regs_t reg)
{
        outb(data, chip->base_addr + reg);
}

/**
 * sja1000_inb
 *
 * macros for reading a byte from a sja1000 register
 *
 **/

static inline uint8_t sja1000_inb(const struct can_chip_t *chip,
                                  enum sja1000_PeliCAN_regs_t reg)
{
        return inb(chip->base_addr + reg);
}

/**
 * sja1000_set_reset_mode
 *
 * set the chip in RESET mode, which is necessary to enable chip configuration.
 * In the original simple driver it was 'sja1000_outb(chip, sjaMOD_RM, SJA_MOD)'
 * but in Lincan they also disable interrupts and perform several tries so it
 * has been added here.
 *
 **/

static int sja1000_set_reset_mode (const struct can_chip_t *chip)
{
        int ret, i;
        uint8_t mode_reg;

        ret = posix_intr_lock (chip->irq);
        if (ret != 0) {
                ERROR("could not disable irq %d\n", chip->irq);
                return ret;
        }

        mode_reg = sja1000_inb (chip, SJA_MOD);
        for (i=0; (!(mode_reg & sjaMOD_RM)) && (i < MX_RETRIES); i++) {
                sja1000_outb (chip, mode_reg | sjaMOD_RM, SJA_MOD);
                mode_reg = sja1000_inb (chip, SJA_MOD);
        }

        if (i == MX_RETRIES) {
                ERROR("too many retries\n");
                posix_intr_unlock(chip->irq);
                return -1;
        }

        DEBUG(SJA1000_ENABLE_DEBUG, "reset mode, ok (mr: 0x%X)\n", mode_reg);
        return 0;
}

/**
 * sja1000_set_operating_mode
 *
 * set the chip in OPERATING mode, which is the normal active state for
 * sending and receiving messages. Like in "sja1000_set_reset_mode" I have
 * followed the Lincan "sja1000p_disable_configuration" procedure, while in
 * the original driver this function consisted only of:
 *
 *        outb(0x00, chip->base_addr + MODE); // for dual filter
 *        outb(0x08, chip->base_addr + MODE); // for single filter
 *
 * The additions to the old driver are enabling interrupts and doing retries.
 * Compared to Lincan's current version we take care of acceptance filters
 * by using "mode_reg & sjaMOD_AFM" when writing to SJA_MOD.
 *
 **/

static int sja1000_set_operating_mode (const struct can_chip_t *chip)
{
        int ret, i;
        uint8_t mode_reg;

        mode_reg = sja1000_inb (chip, SJA_MOD);
        for (i=0; (mode_reg & sjaMOD_RM) && (i < MX_RETRIES); i++) {
                sja1000_outb (chip, (mode_reg & sjaMOD_AFM) , SJA_MOD);
                mode_reg = sja1000_inb (chip, SJA_MOD);
        }

        if (i == MX_RETRIES) {
                ERROR("too many retries\n");
                return -1;
        }

        ret = posix_intr_unlock(chip->irq);
        if (ret != 0) {
                ERROR("could not enable irq %d\n", chip->irq);
                return ret;
        }

        DEBUG(SJA1000_ENABLE_DEBUG, "op. mode, ok (mr: 0x%X)\n", mode_reg);
        return 0;
}

/**
 * sja1000_set_acceptance_filters
 *
 * set the acceptance filter code, mask and mode for the chip.
 *
 * TODO: distinguish standard and extended? and check the function again.
 *
 **/

static int sja1000_set_acceptance_filters(const struct can_chip_t *chip,
                                          const struct can_filter_t *filters,
                                          uint32_t len)
{
        int ret;
        bool chip_was_in_reset_mode;
        uint8_t acc_code_0, acc_code_1, acc_code_2, acc_code_3;
        uint8_t acc_mask_0, acc_mask_1, acc_mask_2, acc_mask_3;
        uint8_t mode_reg;

        mode_reg = sja1000_inb (chip, SJA_MOD);

        // we have to be in RESET mode to change the filter
        chip_was_in_reset_mode = (mode_reg & sjaMOD_RM);

        if (!chip_was_in_reset_mode) {
                ret = sja1000_set_reset_mode (chip);
                if (ret != 0) {
                        ERROR("could not set reset mode\n");
                        return ret;
                }
        }

        mode_reg = sja1000_inb (chip, SJA_MOD);

        switch (len) {
        case 1:
                DEBUG(SJA1000_ENABLE_DEBUG,
                      "set single filter mode (29 ID + rtr + 2 x)\n");
                DEBUG(SJA1000_ENABLE_DEBUG, "code: 0x%X, mask: 0x%X\n",
                                            filters[0].code, filters[0].mask);

                acc_code_0 = (filters[0].code & 0xFF000000) >> 24;
                acc_code_1 = (filters[0].code & 0x00FF0000) >> 16;
                acc_code_2 = (filters[0].code & 0x0000FF00) >> 8;
                acc_code_3 = (filters[0].code & 0x000000FF);

                acc_mask_0 = (filters[0].mask & 0xFF000000) >> 24;
                acc_mask_1 = (filters[0].mask & 0x00FF0000) >> 16;
                acc_mask_2 = (filters[0].mask & 0x0000FF00) >> 8;
                acc_mask_3 = (filters[0].mask & 0x000000FF);

                mode_reg = mode_reg | sjaMOD_AFM;
                sja1000_outb (chip, mode_reg, SJA_MOD);
                DEBUG(SJA1000_ENABLE_DEBUG, "mode_reg AFM: 0x%X\n", mode_reg);

                break;
        case 2:
                DEBUG(SJA1000_ENABLE_DEBUG,
                      "set dual filter mode (16 msb of the ID)\n");
                DEBUG(SJA1000_ENABLE_DEBUG, "filter[0]: 0x%X, mask: 0x%X\n",
                      filters[0].code, filters[0].mask);
                DEBUG(SJA1000_ENABLE_DEBUG, "filter[1]: 0x%X, mask: 0x%X\n",
                      filters[1].code, filters[1].mask);

                acc_code_0 = (filters[0].code >> 21) & 0xFF;
                acc_code_1 = (filters[0].code >> 13) & 0xFF;
                acc_code_2 = (filters[1].code >> 21) & 0xFF;
                acc_code_3 = (filters[1].code >> 13) & 0xFF;

                acc_mask_0 = (filters[0].mask >> 21) & 0xFF;
                acc_mask_1 = (filters[0].mask >> 13) & 0xFF;
                acc_mask_2 = (filters[1].mask >> 21) & 0xFF;
                acc_mask_3 = (filters[1].mask >> 13) & 0xFF;

                mode_reg = mode_reg & (~sjaMOD_AFM);
                sja1000_outb (chip, mode_reg, SJA_MOD);
                DEBUG(SJA1000_ENABLE_DEBUG, "mode_reg AFM: 0x%X\n", mode_reg);

                break;
        default:
                ERROR("num of filters %u not supported\n", len);
                return -1;
        }

        sja1000_outb (chip, acc_code_0, SJA_ACR0);
        sja1000_outb (chip, acc_code_1, SJA_ACR0 + 1);
        sja1000_outb (chip, acc_code_2, SJA_ACR0 + 2);
        sja1000_outb (chip, acc_code_3, SJA_ACR0 + 3);

        sja1000_outb (chip, acc_mask_0, SJA_AMR0);
        sja1000_outb (chip, acc_mask_1, SJA_AMR0 + 1);
        sja1000_outb (chip, acc_mask_2, SJA_AMR0 + 2);
        sja1000_outb (chip, acc_mask_3, SJA_AMR0 + 3);

        if (!chip_was_in_reset_mode) {
                ret = sja1000_set_operating_mode (chip);
                if (ret != 0) {
                        ERROR("could not set operating mode\n");
                        return ret;
                }
        }

        return 0;
}

/**
 * sja1000_set_rate
 *
 * set the rate for the chip. In Lincan this function can handle any value
 * for the rate parameter but for the moment I prefer to use this simple
 * static approach with a set of typical values. The numbers to write at
 * the timing registers are pre-calculated and they have been taken from
 * Juan's original driver. This function must be called while in RESET mode.
 * TODO: put in reset mode if it wasnt like with the acceptance filters
 *
 * NOTE: I should check this cause the values are probably only valid for
 * the xtal of the adlink7841 board. In that case I will have to use the
 * formulas given in the datasheet or use lincan's function.
 *
 **/

static int sja1000_set_rate (const struct can_chip_t *chip)
{
        switch (chip->rate) {
        case 1000:
                DEBUG(SJA1000_ENABLE_DEBUG, "Setting 1000 kbps.\n");
                sja1000_outb (chip, 0x40, SJA_BTR0);
                sja1000_outb (chip, 0x14, SJA_BTR1);
                break;
        case 500:
                DEBUG(SJA1000_ENABLE_DEBUG, "Setting 500 kbps.\n");
                sja1000_outb (chip, 0x40, SJA_BTR0);
                sja1000_outb (chip, 0x3a, SJA_BTR1);
                break;
        case 250:
                DEBUG(SJA1000_ENABLE_DEBUG, "Setting 250 kbps.\n");
                sja1000_outb (chip, 0x41, SJA_BTR0);
                sja1000_outb (chip, 0x3a, SJA_BTR1);
                break;
        case 125:
                DEBUG(SJA1000_ENABLE_DEBUG, "Setting 125 kbps.\n");
                sja1000_outb (chip, 0x43, SJA_BTR0);
                sja1000_outb (chip, 0x3a, SJA_BTR1);
                break;
        case 100:
                DEBUG(SJA1000_ENABLE_DEBUG, "Setting 100 kbps.\n");
                sja1000_outb (chip, 0x83, SJA_BTR0);
                sja1000_outb (chip, 0x4d, SJA_BTR1);
                break;
        case 50:
                DEBUG(SJA1000_ENABLE_DEBUG, "Setting 50 kbps.\n");
                sja1000_outb (chip, 0x87, SJA_BTR0);
                sja1000_outb (chip, 0x4d, SJA_BTR1);
                break;
        case 25:
                printc("Setting 25 kbps.\n");
                sja1000_outb (chip, 0x8f, SJA_BTR0);
                sja1000_outb (chip, 0x4d, SJA_BTR1);
                break;
        case 10:
                DEBUG(SJA1000_ENABLE_DEBUG, "Setting 10 kbps.\n");
                sja1000_outb (chip, 0xa7, SJA_BTR0);
                sja1000_outb (chip, 0x4d, SJA_BTR1);
                break;
        default:
                ERROR("rate %u not supported\n", chip->rate);
                return -1;
        }
        return 0;
}

/**
 * sja1000_report_error
 *
 * This function is called from the IRQ handler when there is an error.
 *
 **/

#if SJA1000_ENABLE_DETAILED_DEBUG

// error codes to strings
static const char *ecc_to_str[]={
        "bit error",
        "form error",
        "stuff error",
        "other type of error"
};

// error codes segments to strings
static const char *eccseg_to_str[]={
        "?0?",
        "?1?",
        "ID.28 to ID.21",
        "start of frame",
        "bit SRTR",
        "bit IDE",
        "ID.20 to ID.18",
        "ID.17 to ID.13",
        "CRC sequence",
        "reserved bit 0",
        "data field",
        "data length code",
        "bit RTR",
        "reserved bit 1",
        "ID.4 to ID.0",
        "ID.12 to ID.5",
        "?16?",
        "active error flag",
        "intermission",
        "tolerate dominant bits",
        "?20?",
        "?21?",
        "passive error flag",
        "error delimiter",
        "CRC delimiter",
        "acknowledge slot",
        "end of frame",
        "acknowledge delimiter",
        "overload flag",
        "?29?",
        "?30?",
        "?31?"
};

#endif

static int sja1000_report_error_limit_counter = 0;

static inline void sja1000_report_error(const struct can_chip_t *chip,
                                        uint8_t sr, uint8_t ir, uint8_t ecc)
{
        uint8_t mr;

        sja1000_report_error_limit_counter++;

        if (sja1000_report_error_limit_counter == 10) {
                ERROR("TOO MANY ERRORS SO REPORTING IS DISABLED\n");
                ERROR("UNTIL THE BUS IS OK (MAYBE THE OTHER NODE IS\n");
                ERROR("NOT SWITCHED ON YET)\n\n");
                return;
        } else if (sja1000_report_error_limit_counter > 10) {
                return;
        }

        mr = sja1000_inb (chip, SJA_MOD);
        ERROR("sr:0x%X ir:0x%02X ecc:0x%02X mr:0x%02X\n", sr, ir, ecc, mr);

#if SJA1000_ENABLE_DETAILED_DEBUG
        ERROR("SR: BS=%c ES=%c TS=%c RS=%c TCS=%c TBS=%c DOS=%c RBS=%c\n",
              (sr & sjaSR_BS)  ? '1' : '0', (sr & sjaSR_ES)  ? '1' :'0',
              (sr & sjaSR_TS)  ? '1' : '0', (sr & sjaSR_RS)  ? '1' :'0',
              (sr & sjaSR_TCS) ? '1' : '0', (sr & sjaSR_TBS) ? '1' :'0',
              (sr & sjaSR_DOS) ? '1' : '0', (sr & sjaSR_RBS) ? '1' :'0');
        ERROR("IR: BEI=%c ALI=%c EPI=%c WUI=%c DOI=%c EI=%c TI=%c RI=%c\n",
              (ir & sjaIR_BEI) ? '1' : '0', (ir & sjaIR_ALI) ? '1' : '0',
              (ir & sjaIR_EPI) ? '1' : '0', (ir & sjaIR_WUI) ? '1' : '0',
              (ir & sjaIR_DOI) ? '1' : '0', (ir & sjaIR_EI)  ? '1' : '0',
              (ir & sjaIR_TI)  ? '1' : '0', (ir & sjaIR_RI)  ? '1' : '0');
        ERROR("EI: %s %s %s\n",
               ecc_to_str[(ecc & (sjaECC_ERCC1 | sjaECC_ERCC0)) / sjaECC_ERCC0],
               (ecc & sjaECC_DIR) ? "RX" : "TX",
               eccseg_to_str[ecc & sjaECC_SEG_M]);
        ERROR("MOD: SM=%c AFM=%c STM=%c LOM=%c RM=%c\n",
              (mr & sjaMOD_SM)  ? '1' : '0', (mr & sjaMOD_AFM)  ? '1' :'0',
              (mr & sjaMOD_STM) ? '1' : '0', (mr & sjaMOD_LOM)  ? '1' :'0',
              (mr & sjaMOD_RM)  ? '1' : '0');
#endif
}

/**
 * sja1000_irq_handler
 *
 * This is the IRQ handler for sja1000 chips. As we have one handler for any
 * of the present sja1000 chips, we have to identify first which is the chip
 * that produced the interrupt (go through the chips with that irq level and
 * read their Interrupt Register). After that, depending on the reason of the
 * interrupt (frame received, error, frame aborted, etc..) we will do the
 * appropiate action.
 *
 **/

static int sja1000_irq_handler (void *area, intr_t irq)
{
        int ret, i;
        bool irq_is_for_me;
        bool is_frame_aborted;
        uint8_t irq_reg, error_reg, status_reg, frm_reg;
        struct can_chip_t *chip;
        struct can_frame_t *frame;
        enum sja1000_PeliCAN_regs_t data;

        DEBUG(SJA1000_ENABLE_DEBUG, "received irq %d\n", irq);

        // check if the interrupt is for me
        irq_is_for_me = false;
        list_for_each_entry(chip, &the_irq_list[irq].irq_list, irq_list) {
                if (chip->chip_type != SJA1000) continue;
                irq_reg = sja1000_inb (chip, SJA_IR);
                if (irq_reg != 0) {
                        irq_is_for_me = true;
                        break;
                }
        }

        // if not for me, maybe other handler for the same irq will handle it
        if (!irq_is_for_me) {
                DEBUG(SJA1000_ENABLE_DEBUG,"received shared irq not for me\n");
                return POSIX_INTR_NOT_HANDLED;
        }

        // TRANSMIT FRAME INTERRUPT handler code
        if (irq_reg & sjaIR_TI) {
                status_reg = sja1000_inb (chip, SJA_SR);
                is_frame_aborted = !(status_reg & sjaSR_TCS);
                if (is_frame_aborted) {
                        DEBUG(SJA1000_ENABLE_DEBUG, "Abort Interrupt\n");
                        if (chip->ops.hook_irq_frame_aborted != NULL) {
                                ret = chip->ops.hook_irq_frame_aborted(chip);
                                if (ret != 0) ERROR("hook_irq_frame_aborted\n");
                        }
                } else {
                        DEBUG(SJA1000_ENABLE_DEBUG, "Transmit Interrupt\n");
                        if (chip->ops.hook_irq_frame_sent != NULL) {
                                ret = chip->ops.hook_irq_frame_sent(chip);
                                if (ret != 0) ERROR("frame_sent_hook error\n");
                        }
                }
        }

        // RECEIVE FRAME INTERRUPT handler code
        if (irq_reg & sjaIR_RI) {
                DEBUG(SJA1000_ENABLE_DEBUG, "Receive Interrupt\n");
                while (sja1000_inb(chip, SJA_SR) & sjaSR_RBS) {
                        // get a free frame and fill it with the data
                        // NOTE: this frame should be freed by the driver's
                        // user when it is not needed anymore
                        frame = can_framespool_alloc();
                        if (frame == NULL) {
                                ERROR("frames pool is exhausted\n");
                                ERROR("Hint: try increasing \
                                                CAN_MX_FRAMES variable\n");
                                return POSIX_INTR_HANDLED_NOTIFY;
                        }

                        frm_reg = sja1000_inb(chip, SJA_FRM);
                        frame->is_extended_format =
                                         (frm_reg & sjaFRM_FF) ? true : false;
                        frame->is_rtr = (frm_reg & sjaFRM_RTR) ? true : false;

                        if (frame->is_extended_format) {
                                frame->id = (sja1000_inb(chip, SJA_ID0) << 21) +
                                            (sja1000_inb(chip, SJA_ID1) << 13) +
                                            (sja1000_inb(chip, SJA_ID2) << 5) +
                                            (sja1000_inb(chip, SJA_ID3) >> 3);
                                data = SJA_DATE;
                        } else {
                                frame->id = (sja1000_inb(chip, SJA_ID0) << 3) +
                                            (sja1000_inb(chip, SJA_ID1) >> 5);
                                data = SJA_DATS;
                        }

                        frame->dlc = frm_reg & sjaFRM_DLC_M;

                        if ((frame->is_rtr) && (frame->dlc != 0)) {
                                WARNING("RTR frame with dlc=%u\n", frame->dlc);
                                frame->dlc = 0;
                        }

                        if (frame->dlc > 8) {
                                WARNING("Received frame with len > 8\n");
                                frame->dlc = 8;
                        }

                        for(i=0; i<frame->dlc; i++) {
                                frame->data[i] = sja1000_inb(chip, data + i);
                        }

                        sja1000_outb(chip, sjaCMR_RRB, SJA_CMR);

                        ret = chip->ops.hook_irq_frame_received(chip, frame);
                        if (ret != 0) ERROR("recv_frame returned error\n");
                }
        }

        // WAKE-UP INTERRUPT handler code (do nothing, we could disable them)
        if (irq_reg & sjaIR_WUI) {
                DEBUG(SJA1000_ENABLE_DEBUG, "Wake-Up Interrupt\n");
        }

        // ERRORs INTERRUPT handler code (TODO: better handling of errors)
        if ((irq_reg & sjaIR_ANYERROR) != 0) {
                error_reg  = sja1000_inb (chip, SJA_ECC);
                status_reg = sja1000_inb (chip, SJA_SR);
                sja1000_report_error(chip, status_reg, irq_reg, error_reg);

                if (sja1000_report_error_limit_counter >= 10) {
                        // allow only non-error interrupts
                        sja1000_outb(chip,
                                     (sjaIER_WUIE | sjaIER_TIE | sjaIER_RIE),
                                     SJA_IER);
                        return POSIX_INTR_HANDLED_NOTIFY;
                }

                if (irq_reg & sjaIR_DOI) {
                        ERROR("data overrun interrupt!!\n");
                        sja1000_outb(chip, sjaCMR_CDO, SJA_CMR);
                }

                if (irq_reg & sjaIR_BEI) {
                        ERROR("bus error interrupt\n");
                        if (status_reg & sjaSR_TS) {
                                ERROR("TS set but we do not abort frame\n");
                        }
                }

                if (irq_reg & sjaIR_ALI) {
                        ERROR("arbitration lost interrupt\n");
                }

                if (irq_reg & sjaIR_EPI) {
                        ERROR("error passive interrupt\n");
                }

                if (irq_reg & sjaIR_EI) {
                        ERROR("error warning interrupt\n");
                        if (status_reg & sjaSR_BS) {
                                ERROR("bus-off recovery not implemented\n");
                        }
                }
        } else {
                if (sja1000_report_error_limit_counter >= 10) {
                        sja1000_report_error_limit_counter = 0;
                        sja1000_outb(chip, sjaENABLE_INTERRUPTS, SJA_IER);
                        DEBUG(true,
                              "TX-RX OK, REPORTING IS ENABLED AGAIN :)\n\n");
                }
        }

        return POSIX_INTR_HANDLED_NOTIFY;
}

/**
 * sja1000_init
 *
 * Initializes a sja1000 chip with the values contained into the structure
 * can_chip_t by writing the appropiate values in the sja1000 registers.
 *
 **/

static int sja1000_init (struct can_chip_t *chip)
{
        int ret;
        bool irq_handler_was_installed;
        struct sj1000_chip_data_t *data;
        struct can_chip_t *pos;
        struct can_filter_t default_filter;

        DEBUG(SJA1000_ENABLE_DEBUG, "start chip configuration\n");

        data = (struct sj1000_chip_data_t *)chip->chip_data;

        // set RESET mode to enable chip configuration
        ret = sja1000_set_reset_mode (chip);
        if (ret != 0) {
                ERROR("could not set reset mode\n");
                return ret;
        }

        // Set clock divider to PeliCAN mode
        sja1000_outb (chip, sjaCDR_PELICAN | data->sja1000_cdr_reg, SJA_CDR);

        // Ensure that interrupts are disabled on the chip
        sja1000_outb (chip, sjaDISABLE_INTERRUPTS, SJA_IER);

        // Set driver output configuration
        sja1000_outb (chip, data->sja1000_ocr_reg, SJA_OCR);

        // set the default acceptance filter (promiscuous single filter)
        default_filter.mask = 0xFFFFFFFF;
        default_filter.code = 0x00000000;

        ret = sja1000_set_acceptance_filters(chip, &default_filter, 1);
        if (ret != 0) {
                ERROR("could not set default acceptance filter\n");
                return ret;
        }

        // set the rate of the bus (contained in the chip structure)
        ret = sja1000_set_rate(chip);
        if (ret != 0) {
                ERROR("could not set rate of the chip\n");
                return ret;
        }

        // install the interrupt handler if it was not installed for this irq
        irq_handler_was_installed = false;
        list_for_each_entry(pos, &the_irq_list[chip->irq].irq_list, irq_list) {
                if (pos->chip_type == SJA1000) {
                        irq_handler_was_installed = true;
                        break;
                }
        }

        if (!irq_handler_was_installed) {
                DEBUG(SJA1000_ENABLE_DEBUG, "set irq %d handler\n", chip->irq);
                ret=posix_intr_associate(chip->irq,sja1000_irq_handler,NULL,0);
                if (ret != 0) {
                        ERROR("could not install irq %d handler\n", chip->irq);
                        return ret;
                }
        } else {
                DEBUG(SJA1000_ENABLE_DEBUG,
                      "handler for irq %d was already installed\n", chip->irq);
        }

        list_add_tail(&chip->irq_list, &the_irq_list[chip->irq].irq_list);

        // Enable hardware interrupts on the chip (NOTE: juan used 0xFF)
        sja1000_outb (chip, sjaENABLE_INTERRUPTS, SJA_IER);

        // after the configuration, exit RESET mode  and set OPERATING mode
        ret = sja1000_set_operating_mode (chip);
        if (ret != 0) {
                ERROR("could not set operating mode\n");
                return ret;
        }

        DEBUG(SJA1000_ENABLE_DEBUG, "chip configuration finished ok\n");
        return 0;
}

/**
 * sja1000_send_frame
 *
 * Send a frame through the sja1000 chip. First we have to distinguish between
 * standard frames (11 bit id) and extended frames (29 bits ids) because
 * the register layout is different.
 *
 **/

static int sja1000_send_frame (const struct can_chip_t *chip,
                               const struct can_frame_t *frame)
{
        int i, ret;
        uint8_t status, ecc, irq_reg;
        uint32_t id;

        // some checkings TODO: define a FULL_CHECKING_ENABLE macro
        if (frame->dlc > 8) {
                ERROR("data lenght code is too big, dlc: %u\n", frame->dlc);
                return -1;
        }

        if (frame->is_rtr && (frame->dlc != 0)) {
                ERROR("dlc must be 0 for a rtr frame\n");
                return -1;
        }

        // Wait until Transmit Buffer Status is released
        status = sja1000_inb (chip, SJA_SR);
        for (i=0; !(status & sjaSR_TBS) && (i < MX_RETRIES); i++) {
                status = sja1000_inb (chip, SJA_SR);
        }

        if (i == MX_RETRIES) {
                ERROR("transmit buffer is locked\n");
                // NOTE: in lincan they abort the frame transmitted, in our
                // case this should never happen cause the application
                // controls when the frame is sent with 'hook_irq_frame_sent'
                return -1;
        }

        // Check for errors in the bus TODO: try to recover from them if we can
        if (status & sjaSR_BS ) {
                ERROR("bus-off status: 0x%X\n", status);
                return -1;
        }

        if (status & sjaSR_ES) {
                ERROR("bus error status: 0x%X\n", status);
                ecc = sja1000_inb (chip, SJA_ECC);
                irq_reg = sja1000_inb (chip, SJA_IR);

                sja1000_report_error(chip, status, irq_reg, ecc);

                ret = sja1000_set_reset_mode (chip);
                if (ret != 0) {
                        ERROR("could not set reset mode\n");
                        return ret;
                }

                sja1000_outb (chip, 0x00, SJA_RXERR);
                sja1000_outb (chip, 0x00, SJA_TXERR);

                ret = sja1000_set_operating_mode (chip);
                if (ret != 0) {
                        ERROR("could not set operating mode\n");
                        return ret;
                }

                status = sja1000_inb (chip, SJA_SR);
                if (status & sjaSR_ES) {
                        ERROR("could not recover\n");
                        ecc = sja1000_inb (chip, SJA_ECC);
                        irq_reg = sja1000_inb (chip, SJA_IR);
                        sja1000_report_error(chip, status, irq_reg, ecc);
                }
        }

        // Write the frame into the tx buffer (we have to distinguish between
        // standard and extended frame formats)
        sja1000_outb (chip, ((frame->is_extended_format) ? sjaFRM_FF : 0) |
                            ((frame->is_rtr) ? sjaFRM_RTR : 0) |
                            frame->dlc, SJA_FRM);

        if (frame->is_extended_format) {
                id = frame->id << 3;
                sja1000_outb (chip, id & 0xff, SJA_ID3);
                id = id >> 8;
                sja1000_outb (chip, id & 0xff, SJA_ID2);
                id = id >> 8;
                sja1000_outb (chip, id & 0xff, SJA_ID1);
                id = id >> 8;
                sja1000_outb (chip, id, SJA_ID0);
                for(i=0; i < frame->dlc; i++) {
                        sja1000_outb (chip, frame->data[i], SJA_DATE + i);
                }
        } else {
                id = frame->id << 5;
                sja1000_outb (chip, id & 0xff, SJA_ID1);
                id = id >> 8;
                sja1000_outb (chip, id & 0xff, SJA_ID0);
                for(i=0; i < frame->dlc; i++) {
                        sja1000_outb (chip, frame->data[i], SJA_DATS + i);
                }
        }

        // Send the trigger command to transmit the frame
        DEBUG(SJA1000_ENABLE_DEBUG,
              "can frame inserted in chip's tx buffer, send it!\n");
        sja1000_outb (chip, sjaCMR_TR, SJA_CMR);
        return 0;
}

/**
 * sja1000_abort_frame
 *
 * Abort the frame that is being sent in the sja1000 chip.
 *
 **/

static int sja1000_abort_frame (const struct can_chip_t *chip)
{
        DEBUG(SJA1000_ENABLE_DEBUG, "aborting frame in hw buffer\n");
        sja1000_outb (chip, sjaCMR_AT, SJA_CMR);
        return 0;
}

/**
 * sja1000_hook_frame_recv_dummy
 *
 * When a frame is received, this function is called to store the frame in
 * some external queue, for example. By default, the function just print out
 * on the screen the contents of the frame. The function is supposed to be
 * replaced by a user function, that would decode the frame and store it on
 * an appropiate queue, through an Ioctl call. The objective is to
 * avoid copying of data between the driver queues and the network protocol
 * queues.
 *
 **/

static int sja1000_hook_frame_recv_dummy (const struct can_chip_t *chip,
                                          struct can_frame_t *frame)
{
        int i, ret;

        DEBUG(SJA1000_ENABLE_DEBUG, "Received a frame\n");
        DEBUG(SJA1000_ENABLE_DEBUG, "Received %s %s, ID=0x%X, Length=%u\n",
              (frame->is_extended_format) ? "Extended" : "Standard",
              (frame->is_rtr) ? "RTR Frame" : "DATA Frame",
              frame->id,
              frame->dlc);

        for (i=0; i<frame->dlc; i++) {
                DEBUG(SJA1000_ENABLE_DEBUG,
                      "data[%d] = 0x%X;\n", i, frame->data[i]);
        }

        // don't forget to free the frame when we don't need it anymore!!
        ret = can_framespool_free(frame);
        if (ret != 0) return ret;

        return 0;
};

/**
 * sja1000_hook_irq_frame_sent_dummy
 */

static int sja1000_hook_irq_frame_sent_dummy(const struct can_chip_t *chip)
{
        DEBUG(SJA1000_ENABLE_DEBUG, "Frame sent, minor %d\n", chip->minor);
        return 0;
};

/**
 * sja1000_hook_frame_aborted_dummy
 */

static int sja1000_hook_frame_aborted_dummy(const struct can_chip_t *chip)
{
        DEBUG(SJA1000_ENABLE_DEBUG, "Frame aborted, minor %d\n", chip->minor);
        return 0;
};

/**
 * the_sja1000_ops
 *
 * This is the interface for the sja1000 module, which manages a sja1000 chip.
 *
 **/

struct can_chip_ops_t the_sja1000_ops = {
        .init        = sja1000_init,
        .send_frame  = sja1000_send_frame,
        .abort_frame = sja1000_abort_frame,
        .set_acceptance_filters = sja1000_set_acceptance_filters,
        .hook_irq_frame_received = sja1000_hook_frame_recv_dummy,
        .hook_irq_frame_sent = sja1000_hook_irq_frame_sent_dummy,
        .hook_irq_frame_aborted = sja1000_hook_frame_aborted_dummy,
};
