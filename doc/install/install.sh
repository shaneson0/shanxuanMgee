#------------------------------------------------------------------------

######################################################################
# INSTALL ERLANG (R13B-03) 
######################################################################

cd /dist/src/
tar xzf ../dist/otp_src_R13B03.tar.gz
cd otp_src_R13B03/

CHOST="x86_64-pc-linux-gnu" CFLAGS="-march=nocona -O2 -pipe" CXXFLAGS="-march=nocona -O2 -pipe" \
./configure --enable-kernel-poll --enable-hipe --enable-threads --enable-smp-support

make && make install




#--------------------------------------------------------------------------

cd /dist/src/
unzip ../dist/xxtea-1.0.3.zip
cd xxtea-1.0.3/

/usr/local/php/bin/phpize

./configure --enable-xxtea --with-php-config=/usr/local/php/bin/php-config

make && make install

#--------------------------------------------------------------------------



# ��װphp-erlang ��չ

cd /dist/src/
tar xjf ../dist/php-erlang-0.0.3.tar.bz2  
cd erlang-0.0.3/
/usr/local/php/bin/phpize

./configure --enable-erlang --with-php-config=/usr/local/php/bin/php-config \

make && make install


������

In file included from /dist/src/erlang-0.0.3/erlang.c:29:
/dist/src/erlang-0.0.3/php_erlang.h:34:16: error: ei.h: No such file or directory
In file included from /dist/src/erlang-0.0.3/erlang.c:29:
/dist/src/erlang-0.0.3/php_erlang.h:37: error: expected ')' before '*' token
/dist/src/erlang-0.0.3/php_erlang.h:38: error: expected ')' before '*' token
/dist/src/erlang-0.0.3/php_erlang.h:41: error: expected specifier-qualifier-list before 'ei_cnode'
/dist/src/erlang-0.0.3/erlang.c: In function 'php_erlang_init_globals':

#�༭encode.c�ļ�
vim encode.c

����if( Z_TYPE_PP( zp ) != IS_RESOURCE ) { return -1; }

���������һ�д���
TSRMLS_FETCH();

����configure
./configure --enable-erlang --with-php-config=/usr/local/php/bin/php-config

vim Makefile

����ERLANG_SHARED_LIBADD = -lei
�޸�����Ϊ 
ERLANG_SHARED_LIBADD = -L/dist/src/otp_src_R13B03/lib/erl_interface/obj/x86_64-unknown-linux-gnu

make && make install



#--------------------------------------------------------------------------
