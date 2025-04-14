extern const unsigned int  _sp_init;
extern const unsigned int  _sp_end;

unsigned int get_sp_init(){
	return (unsigned int)&_sp_init;
}

unsigned int get_sp_end(){
	return (unsigned int)&_sp_end;
}
