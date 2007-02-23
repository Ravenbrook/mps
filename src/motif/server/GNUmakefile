CC = gcc
LIBS = -lXm -lXt -lX11
CFLAGS = -O
LDFLAGS =

TARGET = motifd
OBJS = main.o server.o translations.o packet.o message.o datatrans.o \
       requests.o callbacks.o widgets.o resources.o tables.o motif.o \
       text.o xmstring.o list.o events.o

include Config

$(TARGET) : $(OBJS)
	$(CC) -o $(TARGET) $(LDFLAGS) $(OBJS) $(LIBS)

tables.o : tables.c StringTable.h ClassTable.h TypeTable.h
	$(CC) $(CFLAGS) -c $<

requests.o : requests.c Interface.h
	$(CC) $(CFLAGS) -c $<

clean:
	-rm -f core $(OBJS) $(TARGET)
