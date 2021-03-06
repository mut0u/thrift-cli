/**
 * Autogenerated by Thrift Compiler (0.9.2)
 *
 * DO NOT EDIT UNLESS YOU ARE SURE THAT YOU KNOW WHAT YOU ARE DOING
 *  @generated
 */
import org.apache.thrift.scheme.IScheme;
import org.apache.thrift.scheme.SchemeFactory;
import org.apache.thrift.scheme.StandardScheme;

import org.apache.thrift.scheme.TupleScheme;
import org.apache.thrift.protocol.TTupleProtocol;
import org.apache.thrift.protocol.TProtocolException;
import org.apache.thrift.EncodingUtils;
import org.apache.thrift.TException;
import org.apache.thrift.async.AsyncMethodCallback;
import org.apache.thrift.server.AbstractNonblockingServer.*;
import java.util.List;
import java.util.ArrayList;
import java.util.Map;
import java.util.HashMap;
import java.util.EnumMap;
import java.util.Set;
import java.util.HashSet;
import java.util.EnumSet;
import java.util.Collections;
import java.util.BitSet;
import java.nio.ByteBuffer;
import java.util.Arrays;
import javax.annotation.Generated;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

@SuppressWarnings({"cast", "rawtypes", "serial", "unchecked"})
@Generated(value = "Autogenerated by Thrift Compiler (0.9.2)", date = "2019-11-15")
public class TestMessage implements org.apache.thrift.TBase<TestMessage, TestMessage._Fields>, java.io.Serializable, Cloneable, Comparable<TestMessage> {
  private static final org.apache.thrift.protocol.TStruct STRUCT_DESC = new org.apache.thrift.protocol.TStruct("TestMessage");

  private static final org.apache.thrift.protocol.TField M1_FIELD_DESC = new org.apache.thrift.protocol.TField("m1", org.apache.thrift.protocol.TType.MAP, (short)1);
  private static final org.apache.thrift.protocol.TField M2_FIELD_DESC = new org.apache.thrift.protocol.TField("m2", org.apache.thrift.protocol.TType.MAP, (short)2);

  private static final Map<Class<? extends IScheme>, SchemeFactory> schemes = new HashMap<Class<? extends IScheme>, SchemeFactory>();
  static {
    schemes.put(StandardScheme.class, new TestMessageStandardSchemeFactory());
    schemes.put(TupleScheme.class, new TestMessageTupleSchemeFactory());
  }

  public Map<String,Integer> m1; // required
  public Map<DemoMessage,DemoMessage> m2; // required

  /** The set of fields this struct contains, along with convenience methods for finding and manipulating them. */
  public enum _Fields implements org.apache.thrift.TFieldIdEnum {
    M1((short)1, "m1"),
    M2((short)2, "m2");

    private static final Map<String, _Fields> byName = new HashMap<String, _Fields>();

    static {
      for (_Fields field : EnumSet.allOf(_Fields.class)) {
        byName.put(field.getFieldName(), field);
      }
    }

    /**
     * Find the _Fields constant that matches fieldId, or null if its not found.
     */
    public static _Fields findByThriftId(int fieldId) {
      switch(fieldId) {
        case 1: // M1
          return M1;
        case 2: // M2
          return M2;
        default:
          return null;
      }
    }

    /**
     * Find the _Fields constant that matches fieldId, throwing an exception
     * if it is not found.
     */
    public static _Fields findByThriftIdOrThrow(int fieldId) {
      _Fields fields = findByThriftId(fieldId);
      if (fields == null) throw new IllegalArgumentException("Field " + fieldId + " doesn't exist!");
      return fields;
    }

    /**
     * Find the _Fields constant that matches name, or null if its not found.
     */
    public static _Fields findByName(String name) {
      return byName.get(name);
    }

    private final short _thriftId;
    private final String _fieldName;

    _Fields(short thriftId, String fieldName) {
      _thriftId = thriftId;
      _fieldName = fieldName;
    }

    public short getThriftFieldId() {
      return _thriftId;
    }

    public String getFieldName() {
      return _fieldName;
    }
  }

  // isset id assignments
  public static final Map<_Fields, org.apache.thrift.meta_data.FieldMetaData> metaDataMap;
  static {
    Map<_Fields, org.apache.thrift.meta_data.FieldMetaData> tmpMap = new EnumMap<_Fields, org.apache.thrift.meta_data.FieldMetaData>(_Fields.class);
    tmpMap.put(_Fields.M1, new org.apache.thrift.meta_data.FieldMetaData("m1", org.apache.thrift.TFieldRequirementType.DEFAULT, 
        new org.apache.thrift.meta_data.MapMetaData(org.apache.thrift.protocol.TType.MAP, 
            new org.apache.thrift.meta_data.FieldValueMetaData(org.apache.thrift.protocol.TType.STRING), 
            new org.apache.thrift.meta_data.FieldValueMetaData(org.apache.thrift.protocol.TType.I32))));
    tmpMap.put(_Fields.M2, new org.apache.thrift.meta_data.FieldMetaData("m2", org.apache.thrift.TFieldRequirementType.DEFAULT, 
        new org.apache.thrift.meta_data.MapMetaData(org.apache.thrift.protocol.TType.MAP, 
            new org.apache.thrift.meta_data.FieldValueMetaData(org.apache.thrift.protocol.TType.STRUCT            , "DemoMessage"), 
            new org.apache.thrift.meta_data.FieldValueMetaData(org.apache.thrift.protocol.TType.STRUCT            , "DemoMessage"))));
    metaDataMap = Collections.unmodifiableMap(tmpMap);
    org.apache.thrift.meta_data.FieldMetaData.addStructMetaDataMap(TestMessage.class, metaDataMap);
  }

  public TestMessage() {
  }

  public TestMessage(
    Map<String,Integer> m1,
    Map<DemoMessage,DemoMessage> m2)
  {
    this();
    this.m1 = m1;
    this.m2 = m2;
  }

  /**
   * Performs a deep copy on <i>other</i>.
   */
  public TestMessage(TestMessage other) {
    if (other.isSetM1()) {
      Map<String,Integer> __this__m1 = new HashMap<String,Integer>(other.m1);
      this.m1 = __this__m1;
    }
    if (other.isSetM2()) {
      Map<DemoMessage,DemoMessage> __this__m2 = new HashMap<DemoMessage,DemoMessage>(other.m2.size());
      for (Map.Entry<DemoMessage, DemoMessage> other_element : other.m2.entrySet()) {

        DemoMessage other_element_key = other_element.getKey();
        DemoMessage other_element_value = other_element.getValue();

        DemoMessage __this__m2_copy_key = other_element_key;

        DemoMessage __this__m2_copy_value = other_element_value;

        __this__m2.put(__this__m2_copy_key, __this__m2_copy_value);
      }
      this.m2 = __this__m2;
    }
  }

  public TestMessage deepCopy() {
    return new TestMessage(this);
  }

  @Override
  public void clear() {
    this.m1 = null;
    this.m2 = null;
  }

  public int getM1Size() {
    return (this.m1 == null) ? 0 : this.m1.size();
  }

  public void putToM1(String key, int val) {
    if (this.m1 == null) {
      this.m1 = new HashMap<String,Integer>();
    }
    this.m1.put(key, val);
  }

  public Map<String,Integer> getM1() {
    return this.m1;
  }

  public TestMessage setM1(Map<String,Integer> m1) {
    this.m1 = m1;
    return this;
  }

  public void unsetM1() {
    this.m1 = null;
  }

  /** Returns true if field m1 is set (has been assigned a value) and false otherwise */
  public boolean isSetM1() {
    return this.m1 != null;
  }

  public void setM1IsSet(boolean value) {
    if (!value) {
      this.m1 = null;
    }
  }

  public int getM2Size() {
    return (this.m2 == null) ? 0 : this.m2.size();
  }

  public void putToM2(DemoMessage key, DemoMessage val) {
    if (this.m2 == null) {
      this.m2 = new HashMap<DemoMessage,DemoMessage>();
    }
    this.m2.put(key, val);
  }

  public Map<DemoMessage,DemoMessage> getM2() {
    return this.m2;
  }

  public TestMessage setM2(Map<DemoMessage,DemoMessage> m2) {
    this.m2 = m2;
    return this;
  }

  public void unsetM2() {
    this.m2 = null;
  }

  /** Returns true if field m2 is set (has been assigned a value) and false otherwise */
  public boolean isSetM2() {
    return this.m2 != null;
  }

  public void setM2IsSet(boolean value) {
    if (!value) {
      this.m2 = null;
    }
  }

  public void setFieldValue(_Fields field, Object value) {
    switch (field) {
    case M1:
      if (value == null) {
        unsetM1();
      } else {
        setM1((Map<String,Integer>)value);
      }
      break;

    case M2:
      if (value == null) {
        unsetM2();
      } else {
        setM2((Map<DemoMessage,DemoMessage>)value);
      }
      break;

    }
  }

  public Object getFieldValue(_Fields field) {
    switch (field) {
    case M1:
      return getM1();

    case M2:
      return getM2();

    }
    throw new IllegalStateException();
  }

  /** Returns true if field corresponding to fieldID is set (has been assigned a value) and false otherwise */
  public boolean isSet(_Fields field) {
    if (field == null) {
      throw new IllegalArgumentException();
    }

    switch (field) {
    case M1:
      return isSetM1();
    case M2:
      return isSetM2();
    }
    throw new IllegalStateException();
  }

  @Override
  public boolean equals(Object that) {
    if (that == null)
      return false;
    if (that instanceof TestMessage)
      return this.equals((TestMessage)that);
    return false;
  }

  public boolean equals(TestMessage that) {
    if (that == null)
      return false;

    boolean this_present_m1 = true && this.isSetM1();
    boolean that_present_m1 = true && that.isSetM1();
    if (this_present_m1 || that_present_m1) {
      if (!(this_present_m1 && that_present_m1))
        return false;
      if (!this.m1.equals(that.m1))
        return false;
    }

    boolean this_present_m2 = true && this.isSetM2();
    boolean that_present_m2 = true && that.isSetM2();
    if (this_present_m2 || that_present_m2) {
      if (!(this_present_m2 && that_present_m2))
        return false;
      if (!this.m2.equals(that.m2))
        return false;
    }

    return true;
  }

  @Override
  public int hashCode() {
    List<Object> list = new ArrayList<Object>();

    boolean present_m1 = true && (isSetM1());
    list.add(present_m1);
    if (present_m1)
      list.add(m1);

    boolean present_m2 = true && (isSetM2());
    list.add(present_m2);
    if (present_m2)
      list.add(m2);

    return list.hashCode();
  }

  @Override
  public int compareTo(TestMessage other) {
    if (!getClass().equals(other.getClass())) {
      return getClass().getName().compareTo(other.getClass().getName());
    }

    int lastComparison = 0;

    lastComparison = Boolean.valueOf(isSetM1()).compareTo(other.isSetM1());
    if (lastComparison != 0) {
      return lastComparison;
    }
    if (isSetM1()) {
      lastComparison = org.apache.thrift.TBaseHelper.compareTo(this.m1, other.m1);
      if (lastComparison != 0) {
        return lastComparison;
      }
    }
    lastComparison = Boolean.valueOf(isSetM2()).compareTo(other.isSetM2());
    if (lastComparison != 0) {
      return lastComparison;
    }
    if (isSetM2()) {
      lastComparison = org.apache.thrift.TBaseHelper.compareTo(this.m2, other.m2);
      if (lastComparison != 0) {
        return lastComparison;
      }
    }
    return 0;
  }

  public _Fields fieldForId(int fieldId) {
    return _Fields.findByThriftId(fieldId);
  }

  public void read(org.apache.thrift.protocol.TProtocol iprot) throws org.apache.thrift.TException {
    schemes.get(iprot.getScheme()).getScheme().read(iprot, this);
  }

  public void write(org.apache.thrift.protocol.TProtocol oprot) throws org.apache.thrift.TException {
    schemes.get(oprot.getScheme()).getScheme().write(oprot, this);
  }

  @Override
  public String toString() {
    StringBuilder sb = new StringBuilder("TestMessage(");
    boolean first = true;

    sb.append("m1:");
    if (this.m1 == null) {
      sb.append("null");
    } else {
      sb.append(this.m1);
    }
    first = false;
    if (!first) sb.append(", ");
    sb.append("m2:");
    if (this.m2 == null) {
      sb.append("null");
    } else {
      sb.append(this.m2);
    }
    first = false;
    sb.append(")");
    return sb.toString();
  }

  public void validate() throws org.apache.thrift.TException {
    // check for required fields
    // check for sub-struct validity
  }

  private void writeObject(java.io.ObjectOutputStream out) throws java.io.IOException {
    try {
      write(new org.apache.thrift.protocol.TCompactProtocol(new org.apache.thrift.transport.TIOStreamTransport(out)));
    } catch (org.apache.thrift.TException te) {
      throw new java.io.IOException(te);
    }
  }

  private void readObject(java.io.ObjectInputStream in) throws java.io.IOException, ClassNotFoundException {
    try {
      read(new org.apache.thrift.protocol.TCompactProtocol(new org.apache.thrift.transport.TIOStreamTransport(in)));
    } catch (org.apache.thrift.TException te) {
      throw new java.io.IOException(te);
    }
  }

  private static class TestMessageStandardSchemeFactory implements SchemeFactory {
    public TestMessageStandardScheme getScheme() {
      return new TestMessageStandardScheme();
    }
  }

  private static class TestMessageStandardScheme extends StandardScheme<TestMessage> {

    public void read(org.apache.thrift.protocol.TProtocol iprot, TestMessage struct) throws org.apache.thrift.TException {
      org.apache.thrift.protocol.TField schemeField;
      iprot.readStructBegin();
      while (true)
      {
        schemeField = iprot.readFieldBegin();
        if (schemeField.type == org.apache.thrift.protocol.TType.STOP) { 
          break;
        }
        switch (schemeField.id) {
          case 1: // M1
            if (schemeField.type == org.apache.thrift.protocol.TType.MAP) {
              {
                org.apache.thrift.protocol.TMap _map0 = iprot.readMapBegin();
                struct.m1 = new HashMap<String,Integer>(2*_map0.size);
                String _key1;
                int _val2;
                for (int _i3 = 0; _i3 < _map0.size; ++_i3)
                {
                  _key1 = iprot.readString();
                  _val2 = iprot.readI32();
                  struct.m1.put(_key1, _val2);
                }
                iprot.readMapEnd();
              }
              struct.setM1IsSet(true);
            } else { 
              org.apache.thrift.protocol.TProtocolUtil.skip(iprot, schemeField.type);
            }
            break;
          case 2: // M2
            if (schemeField.type == org.apache.thrift.protocol.TType.MAP) {
              {
                org.apache.thrift.protocol.TMap _map4 = iprot.readMapBegin();
                struct.m2 = new HashMap<DemoMessage,DemoMessage>(2*_map4.size);
                DemoMessage _key5;
                DemoMessage _val6;
                for (int _i7 = 0; _i7 < _map4.size; ++_i7)
                {
                  _key5 = new DemoMessage();
                  _key5.read(iprot);
                  _val6 = new DemoMessage();
                  _val6.read(iprot);
                  struct.m2.put(_key5, _val6);
                }
                iprot.readMapEnd();
              }
              struct.setM2IsSet(true);
            } else { 
              org.apache.thrift.protocol.TProtocolUtil.skip(iprot, schemeField.type);
            }
            break;
          default:
            org.apache.thrift.protocol.TProtocolUtil.skip(iprot, schemeField.type);
        }
        iprot.readFieldEnd();
      }
      iprot.readStructEnd();

      // check for required fields of primitive type, which can't be checked in the validate method
      struct.validate();
    }

    public void write(org.apache.thrift.protocol.TProtocol oprot, TestMessage struct) throws org.apache.thrift.TException {
      struct.validate();

      oprot.writeStructBegin(STRUCT_DESC);
      if (struct.m1 != null) {
        oprot.writeFieldBegin(M1_FIELD_DESC);
        {
          oprot.writeMapBegin(new org.apache.thrift.protocol.TMap(org.apache.thrift.protocol.TType.STRING, org.apache.thrift.protocol.TType.I32, struct.m1.size()));
          for (Map.Entry<String, Integer> _iter8 : struct.m1.entrySet())
          {
            oprot.writeString(_iter8.getKey());
            oprot.writeI32(_iter8.getValue());
          }
          oprot.writeMapEnd();
        }
        oprot.writeFieldEnd();
      }
      if (struct.m2 != null) {
        oprot.writeFieldBegin(M2_FIELD_DESC);
        {
          oprot.writeMapBegin(new org.apache.thrift.protocol.TMap(org.apache.thrift.protocol.TType.STRUCT, org.apache.thrift.protocol.TType.STRUCT, struct.m2.size()));
          for (Map.Entry<DemoMessage, DemoMessage> _iter9 : struct.m2.entrySet())
          {
            _iter9.getKey().write(oprot);
            _iter9.getValue().write(oprot);
          }
          oprot.writeMapEnd();
        }
        oprot.writeFieldEnd();
      }
      oprot.writeFieldStop();
      oprot.writeStructEnd();
    }

  }

  private static class TestMessageTupleSchemeFactory implements SchemeFactory {
    public TestMessageTupleScheme getScheme() {
      return new TestMessageTupleScheme();
    }
  }

  private static class TestMessageTupleScheme extends TupleScheme<TestMessage> {

    @Override
    public void write(org.apache.thrift.protocol.TProtocol prot, TestMessage struct) throws org.apache.thrift.TException {
      TTupleProtocol oprot = (TTupleProtocol) prot;
      BitSet optionals = new BitSet();
      if (struct.isSetM1()) {
        optionals.set(0);
      }
      if (struct.isSetM2()) {
        optionals.set(1);
      }
      oprot.writeBitSet(optionals, 2);
      if (struct.isSetM1()) {
        {
          oprot.writeI32(struct.m1.size());
          for (Map.Entry<String, Integer> _iter10 : struct.m1.entrySet())
          {
            oprot.writeString(_iter10.getKey());
            oprot.writeI32(_iter10.getValue());
          }
        }
      }
      if (struct.isSetM2()) {
        {
          oprot.writeI32(struct.m2.size());
          for (Map.Entry<DemoMessage, DemoMessage> _iter11 : struct.m2.entrySet())
          {
            _iter11.getKey().write(oprot);
            _iter11.getValue().write(oprot);
          }
        }
      }
    }

    @Override
    public void read(org.apache.thrift.protocol.TProtocol prot, TestMessage struct) throws org.apache.thrift.TException {
      TTupleProtocol iprot = (TTupleProtocol) prot;
      BitSet incoming = iprot.readBitSet(2);
      if (incoming.get(0)) {
        {
          org.apache.thrift.protocol.TMap _map12 = new org.apache.thrift.protocol.TMap(org.apache.thrift.protocol.TType.STRING, org.apache.thrift.protocol.TType.I32, iprot.readI32());
          struct.m1 = new HashMap<String,Integer>(2*_map12.size);
          String _key13;
          int _val14;
          for (int _i15 = 0; _i15 < _map12.size; ++_i15)
          {
            _key13 = iprot.readString();
            _val14 = iprot.readI32();
            struct.m1.put(_key13, _val14);
          }
        }
        struct.setM1IsSet(true);
      }
      if (incoming.get(1)) {
        {
          org.apache.thrift.protocol.TMap _map16 = new org.apache.thrift.protocol.TMap(org.apache.thrift.protocol.TType.STRUCT, org.apache.thrift.protocol.TType.STRUCT, iprot.readI32());
          struct.m2 = new HashMap<DemoMessage,DemoMessage>(2*_map16.size);
          DemoMessage _key17;
          DemoMessage _val18;
          for (int _i19 = 0; _i19 < _map16.size; ++_i19)
          {
            _key17 = new DemoMessage();
            _key17.read(iprot);
            _val18 = new DemoMessage();
            _val18.read(iprot);
            struct.m2.put(_key17, _val18);
          }
        }
        struct.setM2IsSet(true);
      }
    }
  }

}

