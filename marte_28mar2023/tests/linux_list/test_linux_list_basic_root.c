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

static struct list_head the_elements;
static struct list_head new_elements;

int main()
{
        struct element_t *elem_p, *tmp;

#if MARTE_ARCHITECTURE == ARCH_X86
        SERIAL_CONSOLE_INIT();
#endif

        printf("initialize list\n");
        INIT_LIST_HEAD(&the_elements);

        printf("add 3 elements to the list\n");
        elem_p = (struct element_t *)malloc(sizeof(struct element_t));
        elem_p->num = 1;
        list_add(&(elem_p->list), &(the_elements));

        elem_p = (struct element_t *)malloc(sizeof(struct element_t));
        elem_p->num = 2;
        list_add(&(elem_p->list), &(the_elements));

        elem_p = (struct element_t *)malloc(sizeof(struct element_t));
        elem_p->num = 3;
        list_add(&(elem_p->list), &(the_elements));

        printf("traverse the list\n");
        list_for_each_entry(elem_p, &the_elements, list) {
                printf("%d\n", elem_p->num);
        }

        printf("delete element 2\n");
        list_for_each_entry_safe(elem_p, tmp, &the_elements, list) {
                if (elem_p->num == 2) {
                        list_del(&elem_p->list);
                        free(elem_p);
                }
        }

        printf("traverse the list\n");
        list_for_each_entry(elem_p, &the_elements, list) {
                printf("%d\n", elem_p->num);
        }

        printf("initialize list\n");
        INIT_LIST_HEAD(&the_elements);

        printf("add tail 6 elements to the list\n");
        elem_p = (struct element_t *)malloc(sizeof(struct element_t));
        elem_p->num = 1;
        list_add_tail(&(elem_p->list), &(the_elements));

        elem_p = (struct element_t *)malloc(sizeof(struct element_t));
        elem_p->num = 2;
        list_add_tail(&(elem_p->list), &(the_elements));

        elem_p = (struct element_t *)malloc(sizeof(struct element_t));
        elem_p->num = 3;
        list_add_tail(&(elem_p->list), &(the_elements));

        elem_p = (struct element_t *)malloc(sizeof(struct element_t));
        elem_p->num = 4;
        list_add_tail(&(elem_p->list), &(the_elements));

        elem_p = (struct element_t *)malloc(sizeof(struct element_t));
        elem_p->num = 5;
        list_add_tail(&(elem_p->list), &(the_elements));

        elem_p = (struct element_t *)malloc(sizeof(struct element_t));
        elem_p->num = 6;
        list_add_tail(&(elem_p->list), &(the_elements));

        printf("traverse the list\n");
        list_for_each_entry(elem_p, &the_elements, list) {
                printf("%d\n", elem_p->num);
        }

        printf("move list to new elements\n");
        INIT_LIST_HEAD(&new_elements);

        list_splice(&the_elements, &new_elements);

        INIT_LIST_HEAD(&the_elements);

        printf("traverse the list\n");
        list_for_each_entry(elem_p, &new_elements, list) {
                printf("%d\n", elem_p->num);
        }

        printf("\n");
        printf("Test OK\n\n");
        return 0;
}
