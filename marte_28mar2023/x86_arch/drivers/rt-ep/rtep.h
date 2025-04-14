#ifndef _RTEP_H_
#define _RTEP_H_

#include <time.h>
#include <stdint.h>

/////////////////////////////////////////////////////////////////////
// TYPES and CONSTANTS
/////////////////////////////////////////////////////////////////////

typedef uint8_t rtep_priority_t;
typedef uint16_t rtep_channel_t;
typedef uint16_t rtep_station_id_t;
typedef uint16_t rtep_server_id_t;
typedef uint16_t rtep_mutex_id_t;
typedef uint16_t rtep_budget_t;

typedef uint8_t rtep_rejection_policy_t;
#define DISCARD_OLDEST  0
#define DISCARD_NEWEST  1

#define MAX_RTEP_MTU 1490
#define MULTICAST_MTU 26

/////////////////////////////////////////////////////////////////////
// RTEP Interface
/////////////////////////////////////////////////////////////////////

extern void rtep_adainit(void);

extern void rtep_adafinal(void);

extern void rtep_init_comm(void);

extern rtep_station_id_t rtep_get_station_id(void);

extern void rtep_send_info
      (rtep_station_id_t station,
       rtep_channel_t channel,
       uint8_t *stream,
       size_t size,
       rtep_priority_t prio);

extern void rtep_server_send_info
      (rtep_station_id_t station,
       rtep_channel_t channel,
       uint8_t *stream,
       size_t size,
       rtep_server_id_t server,
       uint8_t blocking);

extern void rtep_recv_info
      (rtep_station_id_t *station,
       rtep_channel_t channel,
       uint8_t *stream,
       size_t size,
       size_t *last,
       rtep_priority_t *prio);

extern rtep_station_id_t rtep_get_station_id_by_name
      (uint8_t *station_name,
       size_t length);

extern int rtep_server_create
      (rtep_budget_t max_allocated_budget,
       int period_seconds,
       int period_nanoseconds,
       rtep_priority_t prio,
       rtep_server_id_t *server);

extern int rtep_init_distributed_mutex
      (rtep_mutex_id_t mutex_id);

extern int rtep_lock_distributed_mutex
      (rtep_mutex_id_t mutex_id,
       rtep_priority_t prio);

extern int rtep_unlock_distributed_mutex
      (rtep_mutex_id_t mutex_id,
       rtep_priority_t prio);

extern int rtep_finalize_distributed_mutex
      (rtep_mutex_id_t mutex_id);

extern int rtep_valid_multicast_id
      (rtep_station_id_t station);

extern void rtep_rx_queues_set_rejection_policy
      (rtep_channel_t          channel,
       rtep_rejection_policy_t policy);

extern void rtep_fp_tx_queue_set_rejection_policy
      (rtep_rejection_policy_t policy);

extern void rtep_server_tx_queues_set_rejection_policy
      (rtep_server_id_t        server,
       rtep_rejection_policy_t policy);

#endif // _RTEP_H_
