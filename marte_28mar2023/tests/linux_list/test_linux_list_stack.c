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
        struct list_head *pos, *pos_tmp;

#if MARTE_ARCHITECTURE == ARCH_X86
        SERIAL_CONSOLE_INIT();
#endif

        printf("initialize the stack\n");
        INIT_LIST_HEAD(&the_elements.list);

        printf("push 3 elements to the stack\n");
        elem_p = (struct element_t *)malloc(sizeof(struct element_t));
        elem_p->num = 1;
        list_add(&(elem_p->list), &(the_elements.list));

        elem_p = (struct element_t *)malloc(sizeof(struct element_t));
        elem_p->num = 2;
        list_add(&(elem_p->list), &(the_elements.list));

        elem_p = (struct element_t *)malloc(sizeof(struct element_t));
        elem_p->num = 3;
        list_add(&(elem_p->list), &(the_elements.list));

        printf("traverse the list\n");
        list_for_each_entry(elem_p, &the_elements.list, list) {
                printf("%d\n", elem_p->num);
        }

        printf("pop one element\n");
        list_for_each_safe(pos, pos_tmp, &the_elements.list){
                tmp = list_entry(pos, struct element_t, list);
                list_del(pos);
                break;
        }

        printf("element extracted: %d\n", tmp->num);

        printf("traverse the list\n");
        list_for_each_entry(elem_p, &the_elements.list, list) {
                printf("%d\n", elem_p->num);
        }

        if (tmp->num != 3) {
                return -1;
        }

        printf("pop one element\n");
        list_for_each_safe(pos, pos_tmp, &the_elements.list){
                tmp = list_entry(pos, struct element_t, list);
                list_del(pos);
                break;
        }

        printf("element extracted: %d\n", tmp->num);

        printf("traverse the list\n");
        list_for_each_entry(elem_p, &the_elements.list, list) {
                printf("%d\n", elem_p->num);
        }

        if (tmp->num != 2) {
                return -1;
        }

        printf("pop one element\n");
        list_for_each_safe(pos, pos_tmp, &the_elements.list){
                tmp = list_entry(pos, struct element_t, list);
                list_del(pos);
                break;
        }

        printf("element extracted: %d\n", tmp->num);

        printf("traverse the list\n");
        list_for_each_entry(elem_p, &the_elements.list, list) {
                printf("%d\n", elem_p->num);
        }

        if (tmp->num != 1) {
                return -1;
        }

        printf("\n");
        printf("Test OK\n\n");
        return 0;
}
