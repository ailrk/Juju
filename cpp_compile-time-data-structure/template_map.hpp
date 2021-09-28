#include <iostream>
#include <tuple>

// compile time map facility
//
// constexpr map
template <typename T> struct tag_type { using type = T; };

template <auto val> struct tag_auto {
    constexpr static decltype(val) value = val;
};

template <typename key_tag, typename value_tag> struct cpair {
    using key = key_tag;
    using value = value_tag;
};

template <typename... Cpairs> struct cexpr_map {
    template <typename key_tag, std::size_t iter = 0>
    constexpr static auto Find() {
        if constexpr (iter == sizeof...(Cpairs)) {
            return cpair<key_tag, void>();
        } else {
            using current_pair =
                std::tuple_element_t<iter, std::tuple<Cpairs...>>;
            if constexpr (std::is_same_v<key_tag, typename current_pair::key>) {
                return current_pair();
            } else {
                return Find<key_tag, iter + 1>();
            }
        }
    }

  public:
    template <typename key_tag> using find = decltype(Find<key_tag>());
};

enum Instruction {
    NIL,
    LDC,
    LD,
    CAR,
    CDR,
    ATOM,
    ADD,
    SUB,
    CONS,
    EQ,
    GTE,
    LTE,
    SEL,
    JOIN,
    LDF,
    AP,
    RET,
    DUM,
    RAP,
    INSTRUCTION_SENTINEL,
};

constexpr int instruction_num() {
    return static_cast<int>(Instruction::INSTRUCTION_SENTINEL);
}

struct InstructionInfo {
    int32_t arity;
};

using InstructionInfoTable =
    cexpr_map<cpair<tag_auto<NIL>, tag_auto<InstructionInfo{ .arity = 0 }>>,  //
              cpair<tag_auto<LDC>, tag_auto<InstructionInfo{ .arity = 1 }>>,  //
              cpair<tag_auto<LD>, tag_auto<InstructionInfo{ .arity = 0 }>>,   //
              cpair<tag_auto<CAR>, tag_auto<InstructionInfo{ .arity = 1 }>>,  //
              cpair<tag_auto<CDR>, tag_auto<InstructionInfo{ .arity = 1 }>>,  //
              cpair<tag_auto<ATOM>, tag_auto<InstructionInfo{ .arity = 1 }>>, //
              cpair<tag_auto<ADD>, tag_auto<InstructionInfo{ .arity = 2 }>>,  //
              cpair<tag_auto<SUB>, tag_auto<InstructionInfo{ .arity = 2 }>>,  //
              cpair<tag_auto<CONS>, tag_auto<InstructionInfo{ .arity = 2 }>>, //
              cpair<tag_auto<EQ>, tag_auto<InstructionInfo{ .arity = 2 }>>,   //
              cpair<tag_auto<GTE>, tag_auto<InstructionInfo{ .arity = 2 }>>,  //
              cpair<tag_auto<SEL>, tag_auto<InstructionInfo{ .arity = 2 }>>,  //
              cpair<tag_auto<JOIN>, tag_auto<InstructionInfo{ .arity = 0 }>>, //
              cpair<tag_auto<LDF>, tag_auto<InstructionInfo{ .arity = 1 }>>,  //
              cpair<tag_auto<AP>, tag_auto<InstructionInfo{ .arity = 0 }>>,   //
              cpair<tag_auto<RET>, tag_auto<InstructionInfo{ .arity = 0 }>>,  //
              cpair<tag_auto<DUM>, tag_auto<InstructionInfo{ .arity = 0 }>>,  //
              cpair<tag_auto<RAP>, tag_auto<InstructionInfo{ .arity = 0 }>>   //
              >;

// compile time key value lookup

static_assert(InstructionInfoTable::find<tag_auto<LDC>>::value::value.arity ==
              InstructionInfoTable::find<tag_auto<LDC>>::value::value.arity ==
              1);
