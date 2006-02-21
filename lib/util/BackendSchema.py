# -*- coding: utf-8 -*-
# $Id$

from types import ListType, TupleType, StringType

_field_count = 0
_test_count = 0

class TestEnv:
    """
    Testing environment
    """
    _properties = {
        'simple' : None,
        'permutation' : None,
        'syntax': None,
        'semantic': None,
        'schemata' : 'default',
        'type' : None,
        }

    def __init__(self, name=None, **kwargs):
        """
        Assign name to __name__. Add properties and passed-in
        keyword args to __dict__.
        """
        if name is None:
            global _test_count
            _test_count += 1
            name = 'test.%s' % _test_count

        self.__name__ = name

        self.__dict__.update(self._properties)
        self.__dict__.update(kwargs)

    def getName(self):
        """Return the name of this field as a string"""
        return self.__name__

    def getPropertyNames(self):
        return dict(vars(self)).keys()

    def getProperty(self, name):
        return dict(vars(self))[name]

    def __repr__(self):
        """
        Return a string representation.
        """
        return "<TestEnv>"


class InputField:
    """
    Class attribute _properties is a dictionary containing all of a
    field's property values.
    """

    _properties = {
        'required' : False,
        'mode' : 'rw',
        'type' : None,
        'languageIndependent' : False,
        'schemata' : 'default',
        }

    def __init__(self, name=None, **kwargs):
        """
        Assign name to __name__. Add properties and passed-in
        keyword args to __dict__.
        """
        if name is None:
            global _field_count
            _field_count += 1
            name = 'field.%s' % _field_count

        self.__name__ = name

        self.__dict__.update(self._properties)
        self.__dict__.update(kwargs)

    
    def getName(self):
        """Return the name of this field as a string"""
        return self.__name__
    
    def getPropertyNames(self):
        return dict(vars(self)).keys()

    def getProperty(self, name):
        return dict(vars(self))[name]

    def copy(self, name=None):
        """
        Return a copy of field instance, consisting of field name and
        properties dictionary. field name can be changed to given name.
        """
        cdict = dict(vars(self))
        cdict.pop('__name__')
        properties = deepcopy(cdict)
        name = name is not None and name or self.getName()
        return self.__class__(name, **properties)


    def toString(self):
        """Utility method for converting a Field to a string for the
        purpose of comparing fields.  Right now it's pretty crude."""

        retval = '%s(%s): {' % ( self.__class__.__name__, self.__name__ )

        sorted_keys = self._properties.keys()
        sorted_keys.sort()

        for k in sorted_keys:
            value = getattr(self, k, self._properties[k])
            retval = retval + '%s:%s,' % (k, value )

        retval = retval + '}'

        return retval


    def __repr__(self):
        """
        Return a string representation consisting of name, type and permissions.
        """
        return "<InputField %s(%s:%s)>" % (self.getName(), self.type, self.mode)




class Schemata:
    """
    Manage a list of fields by grouping them together.
    Schematas are identified by their names.
    """

    #__implements__ = ISchemata

    def __init__(self, name='default', fields=None):
        """Initialize Schemata and add optional fields."""

        self.__name__ = name
        self._names = []
        self._fields = {}

        if fields is not None:
            if type(fields) not in [ListType, TupleType]:
                fields = (fields, )

            for field in fields:
                self.addField(field)


    def getName(self):
        """Returns the Schemata's name."""
        return self.__name__


    def __add__(self, other):
        """
        Returns a new Schemata object that contains all fields and layers
        from ``self`` and ``other``.
        """

        c = Schemata()
        for field in self.fields():
            c.addField(field)
        for field in other.fields():
            c.addField(field)

        return c


    def copy(self):
        """Returns a deep copy of this Schemata.
        """
        c = Schemata()
        for field in self.fields():
            c.addField(field.copy())
        return c


    def fields(self):
        """Returns a list of my fields in order of their indices."""
        return [self._fields[name] for name in self._names]


    values = fields

    def filterFields(self, *predicates, **values):
        """Returns a subset of self.fields(), containing only fields that
        satisfy the given conditions.

        You can either specify predicates or values or both. If you provide
        both, all conditions must be satisfied.

        For each ``predicate`` (positional argument), ``predicate(field)`` must
        return 1 for a Field ``field`` to be returned as part of the result.

        Each ``attr=val`` function argument defines an additional predicate:
        A field must have the attribute ``attr`` and field.attr must be equal
        to value ``val`` for it to be in the returned list.
        """

        results = []

        for field in self.fields(): # step through each of my fields

            # predicate failed:
            failed = [pred for pred in predicates if not pred(field)]
            if failed: continue

            # attribute missing:
            missing_attrs = [attr for attr in values.keys() \
                             if not shasattr(field, attr)]
            if missing_attrs: continue

            # attribute value unequal:
            diff_values = [attr for attr in values.keys() \
                           if getattr(field, attr) != values[attr]]
            if diff_values: continue

            results.append(field)

        return results


    def __setitem__(self, name, field):
        assert name == field.getName()
        self.addField(field)


    def addField(self, field):
        """Adds a given field to my dictionary of fields."""
        #field = aq_base(field)
        self._validateOnAdd(field)
        name = field.getName()
        if name not in self._names:
            self._names.append(name)
        self._fields[name] = field


    def _validateOnAdd(self, field):
        """Validates fields on adding and bootstrapping
        """

        name = field.getName()
        # two primary fields are forbidden
        if getattr(field, 'primary', False):
            res = self.hasPrimary()
            if res is not False and name != res.getName():
                raise SchemaException("Tried to add '%s' as primary field " \
                         "but %s already has the primary field '%s'." % \
                         (name, repr(self), res.getName())
                      )
        # Do not allowed unqualified references
        if field.type in ('reference', ):
            relationship = getattr(field, 'relationship', '')
            if type(relationship) is not StringType or len(relationship) == 0:
                raise ReferenceException("Unqualified relationship or "\
                          "unsupported relationship var type in field '%s'. "\
                          "The relationship qualifer must be a non empty "\
                          "string." % name
                      )


    def __delitem__(self, name):
        if not self._fields.has_key(name):
            raise KeyError("Schemata has no field '%s'" % name)
        del self._fields[name]
        self._names.remove(name)


    def __getitem__(self, name):
        return self._fields[name]


    def get(self, name, default=None):
        return self._fields.get(name, default)


    def has_key(self, name):
        return self._fields.has_key(name)


    __contains__ = has_key


    def keys(self):
        return self._names


    delField = __delitem__

    updateField = addField

    def searchable(self):
        """Returns a list containing names of all searchable fields."""

        return [f.getName() for f in self.fields() if f.searchable]


    def hasPrimary(self):
        """Returns the first primary field or False"""
        for f in self.fields():
            if getattr(f, 'primary', False):
                return f
        return False


class BasicSchema(Schemata):
    """Manage a list of fields and run methods over them."""

    #__implements__ = ISchema

    _properties = {}

    def __init__(self, *args, **kwargs):
        """
        Initialize a Schema.

        The first positional argument may be a sequence of
        Fields. (All further positional arguments are ignored.)

        Keyword arguments are added to my properties.
        """
        Schemata.__init__(self)

        self._props = self._properties.copy()
        self._props.update(kwargs)
        
        if len(args):
            if type(args[0]) in [ListType, TupleType]:
                for field in args[0]:
                    self.addField(field)
            else:
                msg = ('You are passing positional arguments '
                       'to the Schema constructor. '
                       'Please consult the docstring '
                       'for %s.BasicSchema.__init__' %
                       (self.__class__.__module__,))

                level = 3
                if self.__class__ is not BasicSchema:
                    level = 4
                
                print (msg, level)
                
                for field in args:
                    self.addField(args[0])


    def __add__(self, other):
        c = BasicSchema()
        # We can't use update and keep the order so we do it manually
        for field in self.fields():
            c.addField(field)
        for field in other.fields():
            c.addField(field)
        # Need to be smarter when joining layers
        # and internal props
        c._props.update(self._props)
        return c


    def copy(self):
        """Returns a deep copy of this Schema.
        """
        c = BasicSchema()
        for field in self.fields():
            c.addField(field.copy())
        # Need to be smarter when joining layers
        # and internal props
        c._props.update(self._props)
        return c


    def edit(self, instance, name, value):
        if self.allow(name):
            instance[name] = value


    def setDefaults(self, instance):
        """Only call during object initialization. Sets fields to
        schema defaults
        """
        ## XXX think about layout/vs dyn defaults
        for field in self.values():
            if field.getName().lower() == 'id': continue
            if field.type == "reference": continue

            # always set defaults on writable fields
            mutator = field.getMutator(instance)
            if mutator is None:
                continue
            default = field.getDefault(instance)

            args = (default,)
            kw = {'field': field.__name__,
                  '_initializing_': True}
            if shasattr(field, 'default_content_type'):
                # specify a mimetype if the mutator takes a
                # mimetype argument
                kw['mimetype'] = field.default_content_type
            mapply(mutator, *args, **kw)


    def updateAll(self, instance, **kwargs):
        """This method mutates fields in the given instance.

        For each keyword argument k, the key indicates the name of the
        field to mutate while the value is used to call the mutator.

        E.g. updateAll(instance, id='123', amount=500) will, depending on the
        actual mutators set, result in two calls: ``instance.setId('123')`` and
        ``instance.setAmount(500)``.
        """

        keys = kwargs.keys()

        for name in keys:

            field = self.get(name, None)

            if field is None:
                continue

            if not field.writeable(instance):
                continue

            # If passed the test above, mutator is guaranteed to
            # exist.
            method = field.getMutator(instance)
            method(kwargs[name])


    def allow(self, name):
        return self.has_key(name)


    def toString(self):
        s = '%s: {' % self.__class__.__name__
        for f in self.fields():
            s = s + '%s,' % (f.toString())
        s = s + '}'
        return s


    def signature(self):
        from md5 import md5
        return md5(self.toString()).digest()


    def changeSchemataForField(self, fieldname, schemataname):
        """ change the schemata for a field """
        field = self[fieldname]
        self.delField(fieldname)
        field.schemata = schemataname
        self.addField(field)


    def getSchemataNames(self):
        """Return list of schemata names in order of appearing"""
        lst = []
        for f in self.fields():
            if not f.schemata in lst:
                lst.append(f.schemata)
        return lst


    def getSchemataFields(self, name):
        """Return list of fields belong to schema 'name'
        in order of appearing
        """
        return [f for f in self.fields() if f.schemata == name]


    def replaceField(self, name, field):
        if IField.isImplementedBy(field):
            oidx = self._names.index(name)
            new_name = field.getName()
            self._names[oidx] = new_name
            del self._fields[name]
            self._fields[new_name] = field
        else:
            raise ValueError, "Object doesn't implement IField: %r" % field



class Schema(BasicSchema):
    """
    Schema
    """

    #__implements__ = ILayerRuntime, ILayerContainer, ISchema

    def __init__(self, *args, **kwargs):
        BasicSchema.__init__(self, *args, **kwargs)


    def __add__(self, other):
        c = Schema()
        # We can't use update and keep the order so we do it manually
        for field in self.fields():
            c.addField(field)
        for field in other.fields():
            c.addField(field)
        # Need to be smarter when joining layers
        # and internal props
        c._props.update(self._props)
        layers = {}
        for k, v in self.registeredLayers():
            layers[k] = v
        for k, v in other.registeredLayers():
            layers[k] = v
        for k, v in layers.items():
            c.registerLayer(k, v)
        return c


    def copy(self, factory=None):
        """Returns a deep copy of this Schema.
        """
        if factory is None:
            factory = self.__class__
        c = factory()
        for field in self.fields():
            c.addField(field.copy())
        # Need to be smarter when joining layers
        # and internal props
        c._props.update(self._props)
        layers = {}
        for k, v in self.registeredLayers():
            c.registerLayer(k, v)
        return c


    def wrapped(self, parent):
        schema = self.copy(factory=WrappedSchema)
        return schema.__of__(parent)
        


# -- Test section -------------------------------------------------------------
if __name__ == "__main__":

    simpleSchema = Schema((
        TestEnv(
            simple = 'def test(a, b): return (a == b)',
            semantic = 'import sys\n\n',
        ),

        InputField(
            'modelSolution', 
            required = False, 
            label = 'Model solution',
            description = 'Enter a model solution.',
            i18n_domain = 'EC',
        ),
        
        InputField(
            'properties', 
            required = True, 
            label = 'QuickCheck properties',
            description = 'Enter one or more QuickCheck properties. '+ 
                        'Use #model# as place marker for the module in which '+ 
                        'the model solution will be defined and #student# as '+
                        'place marker for the students\' solution module.',
            i18n_domain = 'EC',
        ),
    ))
    
    #print simpleField.getName()
    #print simpleField.label
    #print simpleField
    #print simpleField.toString()
    
    for field in simpleSchema.fields():
        #print field.getName
        if isinstance(field, TestEnv):
            print 'TestEnv ->', field.getName()
        elif isinstance(field, InputField):
            print 'InputField ->', field.getName()
            