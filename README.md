# krb5

krb5基于最新的krb5-1.14.1版本，修改了部分代码，未经过完整的测试。

#### 编译过程

- 编译前的准备

  终端切换到源码目录

  ```shell
  echo krb5_cv_attr_constructor_destructor=yes>linux-cache && echo ac_cv_func_regcomp=yes>>linux-cache && echo ac_cv_printf_positional=yes>>linux-cache && echo ac_cv_file__etc_environment=yes>>linux-cache && echo ac_cv_file__etc_TIMEZONE=yes>>linux-cache
  ```


- configure

  ```shell
  ./configure --prefix=/usr/local/arm/krb5 --host=arm-linux-androideabi --cache-file=linux-cache (如果需要修改路径可以使用--prefix)
  ```


- make
- make install

