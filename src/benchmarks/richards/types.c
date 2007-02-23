/* types.c */

#include "types.h"

/*
// AddToList - list utility (append elem at end of list, return head)
*/
Packet *AddToList(Packet *list, Packet *elem)
{   Packet *p, *next;

    elem->SetLink(NoWork);
    if (list != NoWork) {
         p = list;
         while((next = p->Link()) != NoWork) p = next;
         p->SetLink(elem);
    }
    else list = elem;
    return list;
}

Packet::Packet(Packet *l, Identity id, PacketKind k)
{
    link = l;
    ident = id;
    kind = k;
    datum = 1;
    for(int i = 0; i < 4; i++) data[i] = 0;
}
