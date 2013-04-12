# -*- coding: utf-8 -*-
# $Id$
#
# This file is part of ECSpooler.
#

from spyne.model.complex import Array
from spyne.model.complex import ComplexModel
from spyne.model.primitive import Integer
from spyne.model.primitive import Boolean
from spyne.model.primitive import Unicode

#from soaplib.serializers.primitive import Any
#from soaplib.serializers.primitive import AnyAsDict

TARGET_NAMESPACE = 'org.educomponents.spooler.soapwrapper'

# -- DTOs used by spooler and backends ---------------------------------------- 

class State(ComplexModel):
    """
    """
    __namespace__ = TARGET_NAMESPACE
    # process ID
    pid = Integer

class SpoolerState(State):
    """
    """
    __namespace__ = TARGET_NAMESPACE
    # list of attached backends
    backends = Array(Unicode)
    # number of cached jobs
    queueCount = Integer
    # number of cached results
    resultCount = Integer
    
class BackendStatus(ComplexModel):
    
    __namespace__ = TARGET_NAMESPACE
    
    name        = Unicode
    pid         = Integer
    id          = Unicode
    host        = Unicode
    version     = Unicode
    spooler     = Unicode
    port        = Integer
    
class Backend(ComplexModel):
    
    __namespace__ = TARGET_NAMESPACE
    
    url         = Unicode
    isBusy      = Boolean
    version     = Unicode
    id          = Unicode
    name        = Unicode

class BackendInputField(ComplexModel):
    
    __namespace__ = TARGET_NAMESPACE
    
    name                = Unicode
    description         = Unicode
    format              = Unicode
    required            = Boolean
    label               = Unicode
    languageIndependent = Boolean
    i18n_domain         = Unicode
    type                = Unicode
    accessor            = Unicode
    
class BackendTestField(ComplexModel):
    
    __namespace__ = TARGET_NAMESPACE
    
    id   = Unicode
    name = Unicode
    
class DictElem(ComplexModel):
    
    __namespace__ = TARGET_NAMESPACE
    
    key   = Unicode
    value = Unicode
    
class Answer(ComplexModel):
    
    __namespace__ = TARGET_NAMESPACE
    
    id      = Unicode
    value   = Integer
    message = Unicode