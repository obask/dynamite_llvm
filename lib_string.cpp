#ifndef LIB_STRING
#define LIB_STRING


struct StringX {
    Object rtti;
    Object *internal;
};


extern "C"
struct Object* newString(char* ss) {
    struct StringX *data = new struct StringX;
    data->rtti.kind = STRING_KIND;
    data->internal = (Object*)ss;
    return (struct Object *) data;
}


extern "C"
struct Object* StringX_getToken(Object* obj, Object* arg1) {
    cout << "StringX_getToken" << endl;
    return NULL;
}

extern "C"
struct Object* StringX_dump(Object* ss) {
    cout << "StringX_dump" << endl;
    struct StringX *tmp = (struct StringX*)ss;
    printf("%s\n", (char*)tmp->internal);
    return NULL;
}

#endif
