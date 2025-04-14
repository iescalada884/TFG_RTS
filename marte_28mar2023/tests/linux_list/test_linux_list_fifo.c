//  Test for all architectures
#include <stdio.h>  // printf
#include <stdlib.h> // malloc
#include <misc/linux_list.h>
#include <sys/marte_configuration_parameters.h>

#if MARTE_ARCHITECTURE == ARCH_X86
#include <drivers/console_switcher.h>
#endif

struct element_t {
        int num;
        struct list_head list;
};

static struct element_t the_elements;

int main()
{
        struct element_t *elem_p, *tmp;
        struct list_head *pos;

#if MARTE_ARCHITECTURE == ARCH_X86
        SERIAL_CONSOLE_INIT();
#endif

        printf("initialize the fifo\n");
        INIT_LIST_HEAD(&the_elements.list);

        printf("add 3 elements to the fifo\n");
        elem_p = (struct element_t *)malloc(sizeof(struct element_t));
        elem_p->num = 1;
        list_add_tail(&(elem_p->list), &(the_elements.list));

        elem_p = (struct element_t *)malloc(sizeof(struct element_t));
        elem_p->num = 2;
        list_add_tail(&(elem_p->list), &(the_elements.list));

        elem_p = (struct element_t *)malloc(sizeof(struct element_t));
        elem_p->num = 3;
        list_add_tail(&(elem_p->list), &(the_elements.list));

        printf("traverse the list\n");
        list_for_each_entry(elem_p, &the_elements.list, list) {
                printf("%d\n", elem_p->num);
        }

        printf("extract one element\n");
        list_for_each(pos, &the_elements.list){
                tmp = list_entry(pos, struct element_t, list);
                break;
        }
        list_del(pos);

        printf("element extracted: %d\n", tmp->num);

        printf("traverse the list\n");
        list_for_each_entry(elem_p, &the_elements.list, list) {
                printf("%d\n", elem_p->num);
        }

        if (tmp->num != 1) {
                return -1;
        }

        printf("extract another element\n");
        list_for_each(pos, &the_elements.list){
                tmp = list_entry(pos, struct element_t, list);
                break;
        }
        list_del(pos);

        printf("element extracted: %d\n", tmp->num);

        printf("traverse the list\n");
        list_for_each_entry(elem_p, &the_elements.list, list) {
                printf("%d\n", elem_p->num);
        }

        if (tmp->num != 2) {
                return -1;
        }

        printf("extract another element\n");
        list_for_each(pos, &the_elements.list){
                tmp = list_entry(pos, struct element_t, list);
                break;
        }
        list_del(pos);

        printf("element extracted: %d\n", tmp->num);

        printf("traverse the list\n");
        list_for_each_entry(elem_p, &the_elements.list, list) {
                printf("%d\n", elem_p->num);
        }

        if (tmp->num != 3) {
                return -1;
        }

        printf("Test OK\n\n");
        return 0;
}
