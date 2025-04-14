#ifndef _RTEP_BWRES_H_
#define _RTEP_BWRES_H_

/* for timespec */
#include <time.h>
/* for uint*_t */
#include <stdint.h>
/* for rtep_priority_t, rtep_server_id_t, rtep_budget_t */
#include "rtep.h"

/////////////////////////////////////////////////////////////////////
// TYPES and CONSTANTS
/////////////////////////////////////////////////////////////////////

typedef uint16_t rtep_contract_id_t;

typedef struct {
   struct timespec period_max;
   struct timespec deadline;
   rtep_budget_t budget_min;
   rtep_priority_t prio;
} rtep_bwres_contract_t;

typedef struct {
   rtep_contract_id_t contract_id;
   rtep_server_id_t server_id;
} rtep_bwres_vres_t;

#define MAX_N_RTEP_BWRES_VRES 60 /* RTEP.Number_Of_Servers */

/////////////////////////////////////////////////////////////////////
// Bandwith Reservation Interface
/////////////////////////////////////////////////////////////////////

extern int rtep_bwres_init(void);

extern int rtep_bwres_contract_set_basic_params
   (rtep_bwres_contract_t *contract,
    const rtep_budget_t budget_min,
    const struct timespec *period_max,
    const struct timespec *deadline);

extern int rtep_bwres_set_priority
   (rtep_bwres_contract_t *contract,
    const rtep_priority_t prio);

extern int rtep_bwres_contract_negotiate
   (const rtep_bwres_contract_t *contract,
    rtep_bwres_vres_t *vres);

extern int rtep_bwres_vres_destroy
      (const rtep_bwres_vres_t *vres);

extern int rtep_bwres_get_server_id
   (const rtep_bwres_vres_t *vres,
    rtep_server_id_t *server);

extern int rtep_bwres_contract_renegotiate_sync
   (rtep_bwres_vres_t *vres,
    const rtep_bwres_contract_t *contract);

extern int rtep_bwres_renegotiate_negotiation_period
      (const struct timespec *period_max);

extern int rtep_bwres_get_negotiation_period
      (struct timespec *period_max);

#endif // _RTEP_BWRES_H_

