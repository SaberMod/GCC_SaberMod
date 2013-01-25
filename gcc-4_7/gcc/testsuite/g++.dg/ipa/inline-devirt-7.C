/* Verify that the inliner makes good decisions and the example
   is optimized to 4 printf()s in main().  */
// { dg-do compile }
// { dg-options "-O2 -std=c++0x -fdump-tree-optimized"  }

#include <stdio.h>

class Stream
{
public:
	virtual unsigned char read() = 0;
	virtual ~Stream() {}
};

class Connection
{
public:
	virtual void open() = 0;
	virtual void close() = 0;
	virtual ~Connection() {}
};

class Socket : public Stream, public Connection
{
public:
	virtual unsigned char read() = 0;
	virtual void open() = 0;
	virtual void close() { printf("generic close\n"); }
};

class LinuxSocket : public Socket
{
public:
	virtual unsigned char read() { return 'l'; }
	virtual void open() { printf("linux open\n"); }
};

class CustomSocket : public Socket
{
public:
	virtual unsigned char read() { return 0; }
	virtual void open() { printf("custom open\n"); }
	virtual void close() { printf("custom close\n"); }
};

static void readToEnd(Stream* stream)
{
	while (stream->read() == 0) { printf("got it\n"); }
}

static Socket* createSocket()
{
	return new LinuxSocket();
}

int main()
{
	auto socket = createSocket();
	socket->open();
	readToEnd(socket);
	socket->close();

	delete socket;

	return 0;
}

// { dg-final { scan-tree-dump "__builtin_puts \\(&\"linux open\"\\\[0\\\]\\);\n" "optimized" { xfail *-*-* } } }
// { dg-final { scan-tree-dump "__builtin_puts \\(&\"generic close\"\\\[0\\\]\\);\n" "optimized" { xfail *-*-* } } }
// { dg-final { cleanup-tree-dump "optimized" } }
